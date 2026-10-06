{-|
PDF stream optimization with intelligent filter selection

This module performs comprehensive optimization of PDF objects and their
streams.

Optimization includes:

- __Stream-level optimization__: XML formatting, graphics commands, JPEG images,
  TrueType fonts
- __Filter optimization__: Selecting the best compression filter combination
  (Zopfli instead of Zlib, combining Zopfli and RLE, etc.)
- __String optimization__: Removing unnecessary spaces in nested structures
- __Filter support__: Handles FlateDecode, RLEDecode, LZWDecode, ASCII85Decode,
  ASCIIHexDecode, DCTDecode, JPXDecode

Optimization is only applied to objects with supported filters. Unsupported
filters prevent optimization to avoid data corruption. Progress and errors are
reported through the 'PDFWork' monad with contextual information.
-}
module PDF.Processing.Optimize
  ( optimize
  ) where

import Codec.Compression.Predict (unpredict)
import Codec.Compression.Predict.Predictor (decodePredictor)
import Codec.Compression.XML (optimizeXML)

import Control.Exception (IOException, try)
import Control.Monad.State (gets, lift)
import Control.Monad.Trans.Except (runExceptT)

import Data.Binary (Word8)
import Data.Bitmap.Bitmap
  ( Bitmap (bitmapOriginalEncoding, bitmapSamples)
  , BitmapEncoding (JPEGEncoding)
  )
import Data.Bitmap.BitmapConfiguration
  ( BitmapConfiguration (BitmapConfiguration, bcBitsPerComponent, bcComponents, bcLineWidth)
  )
import Data.Bitmap.BitsPerComponent (BitsPerComponent (BC8Bits))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Context (Contextual (ctx))
import Data.IntMap.Strict qualified as IM
import Data.Logging (Logging)
import Data.Maybe (isNothing)
import Data.PDF.Filter (Filter (fFilter))
import Data.PDF.OptimizationType
  ( OptimizationType (GfxOptimization, JPGOptimization, RawBitmapOptimization, TTFOptimization, XMLOptimization)
  )
import Data.PDF.PDFObject
  ( PDFObject (PDFBool, PDFHexString, PDFIndirectObject, PDFIndirectObjectWithStream, PDFName, PDFNumber, PDFObjectStream, PDFTrailer, PDFXRefStream)
  , getObjectNumber
  , hasStream
  , mkPDFArray
  )
import Data.PDF.PDFWork
  (PDFWork, sayComparisonP, sayErrorP, sayP, tryP, withContext)
import Data.PDF.WorkData (wBitmaps)
import Data.Sequence qualified as SQ
import Data.Text qualified as T
import Data.UnifiedError (UnifiedError)

import External.ImageMagick (encodeBitmapJPEG, extractCbCrChannels)
import External.JpegTran (jpegToGrayscale, jpegtranOptimize)
import External.TtfAutoHint (ttfAutoHintOptimize)

import Font.TrueType.FontDirectory (fromFontDirectory, optimizeFontDirectory)
import Font.TrueType.Parser.Font (ttfParse)

import PDF.Graphics.Optimize (optimizeGFXWithTextState)
import PDF.Object.Container (getFilters)
import PDF.Object.Object.Properties (getValueForKey)
import PDF.Object.State
  (getStream, getValue, removeValue, setStream, setStream1, setValue)
import PDF.Object.String (optimizeString)
import PDF.Processing.Filter (filterOptimize)
import PDF.Processing.PDFWork (deepMapP)
import PDF.Processing.Unfilter (unfilter)
import PDF.Processing.WhatOptimizationFor (whatOptimizationFor)

import Util.ByteString
  (compactGray, containsOnlyGray, convertToGray, isNearlyGray, optimizeParity)
import Util.Dictionary (Dictionary)

{-|
Extract width and color component count from a PDF image stream object.

Analyzes a PDF stream object's dictionary to determine its pixel width and the
number of color components per pixel. This information is essential for
bitmap optimization operations that work at the pixel level.

__Color space mapping:__

- DeviceRGB: 3 components (red, green, blue)
- DeviceCMYK: 4 components (cyan, magenta, yellow, black)
- DeviceGray or other: 1 component (grayscale)

__Parameters:__

- A PDF object (typically an image stream)

__Returns:__ 'Just' @(width, components)@ if both width and color space can be
determined, 'Nothing' otherwise.

__Note:__ Non-stream objects always return 'Nothing'.
-}
getBitmapConfiguration :: Logging m => PDFObject -> PDFWork m (Maybe BitmapConfiguration)
getBitmapConfiguration object@(PDFIndirectObjectWithStream _number _version _dict _stream) = do
  mWidth      <- getValue "Width" object
  mColorSpace <- getValue "ColorSpace" object

  let components :: Int
      components = case mColorSpace of
        Just (PDFName "DeviceRGB" ) -> 3
        Just (PDFName "DeviceCMYK") -> 4
        _anyOtherValue              -> 1

  return $ case mWidth of
    Just (PDFNumber width) ->
      let bitmapConfig = BitmapConfiguration
            { bcLineWidth        = round width
            , bcComponents       = components
            , bcBitsPerComponent = BC8Bits
            }
      in Just bitmapConfig
    _anyOtherValue         -> Nothing

getBitmapConfiguration _anyOtherObject = return Nothing

{-|
Optimize 8-bit RGB and grayscale bitmaps. Nearly gray RGB samples are reduced
to one component, then distinct gray levels determine a lossless 1-, 2-, 4-
or 8-bit representation. Arbitrary levels use an Indexed DeviceGray palette.
Rows are byte-aligned. Custom Decode arrays and color-key masks prevent
color-space or sample remapping.
-}
optimizeStreamParity :: PDFObject -> PDFWork IO PDFObject
optimizeStreamParity object = do
  mBitmapConfig <- getBitmapConfiguration object
  mPredictor <- getValue "Predictor" object >>= \case
    Just (PDFNumber n) -> return $ Just (decodePredictor (round n :: Word8))
    _other             -> return Nothing

  stream <- getStream object

  -- Unpredict the stream if needed
  let rawStream = case (mBitmapConfig, mPredictor) of
                    (Just bitmapConfig, Just (Right predictor)) ->
                      case unpredict predictor bitmapConfig stream of
                        (Right predicted)  -> predicted
                        (Left  _err      ) -> stream
                    _anyOtherCase -> stream

  mColorSpace <- getValue "ColorSpace" object
  mBits <- getValue "BitsPerComponent" object
  mDecode <- getValue "Decode" object
  mMask <- getValue "Mask" object
  mHeight <- getValue "Height" object
  mImageMask <- getValue "ImageMask" object

  let
    eightBit :: Bool
    eightBit = isNothing mBits || mBits == Just (PDFNumber 8)

    defaultDecode :: Bool
    defaultDecode = isNothing mDecode

    compact :: PDFObject -> ByteString -> PDFWork IO PDFObject
    compact grayObject grayStream =
      case (mBitmapConfig, mHeight) of
        (Just config, Just (PDFNumber h))
          | defaultDecode && isNothing mMask
          , Just (bits, palette, packed) <-
              compactGray (bcLineWidth config) (round h) grayStream -> do

              let
                colorSpace :: PDFObject
                colorSpace = case palette of
                  Nothing -> PDFName "DeviceGray"

                  Just values -> mkPDFArray
                    [ PDFName "Indexed"
                    , PDFName "DeviceGray"
                    , PDFNumber (fromIntegral (BS.length values - 1))
                    , PDFHexString values
                    ]

              sayComparisonP "Gray bit depth optimization"
                             (BS.length grayStream)
                             (BS.length packed)

              setValue "ColorSpace" colorSpace grayObject
                >>= setValue "BitsPerComponent" (PDFNumber (fromIntegral bits))
                >>= setStream packed

        _other -> setStream grayStream grayObject

  if not eightBit || mImageMask == Just (PDFBool True)
    then
      return object
    else
      case mColorSpace of
        Just (PDFName "DeviceRGB")
          | defaultDecode && isNothing mMask && containsOnlyGray rawStream -> do
            let
              grayStream :: ByteString
              grayStream = convertToGray rawStream

            sayComparisonP "Gray bitmap optimization"
                          (BS.length rawStream)
                          (BS.length grayStream)

            grayObject <- setValue "ColorSpace" (PDFName "DeviceGray") object
            compact grayObject grayStream

          | defaultDecode -> do
              sayP "RGB parity optimization"
              setStream (optimizeParity rawStream) object

        Just (PDFName "DeviceGray") -> compact object rawStream

        _other -> return object

{-|
Convert an RGB JPEG stream to grayscale if it contains only gray pixel values.

Decodes the compressed JPEG to raw RGB pixels to inspect them with
'containsOnlyGray'. When every pixel is gray, losslessly reduces the JPEG to a
single-component grayscale image and updates the object's @ColorSpace@ to
'DeviceGray'.
-}
optimizeJpegColorSpace :: PDFObject -> PDFWork IO PDFObject
optimizeJpegColorSpace object = do
  mColorSpace <- getValue "ColorSpace" object

  if mColorSpace /= Just (PDFName "DeviceRGB")
    then
      return object
    else do
      stream <- getStream object
      tryP (lift $ extractCbCrChannels stream) >>= \case
        Right (rawCb, rawCr) | isNearlyGray rawCb rawCr -> do
          sayP "Image is nearly gray"
          tryP (lift $ jpegToGrayscale stream) >>= \case
            Right grayStream -> do
              sayComparisonP "Gray JPEG conversion"
                             (BS.length stream)
                             (BS.length grayStream)
              setValue "ColorSpace"
                       (PDFName "DeviceGray")
                       object
                >>= setStream grayStream

            Left theError -> do
              sayErrorP "cannot convert JPEG to grayscale" theError
              return object

        Left anError -> do
          sayErrorP "cannot extract CbCr channels" anError
          return object

        _anyOtherCase ->
          return object

{-|
Optimize TrueType font stream data using ttfAutoHint and internal optimization.
-}
optimizeTTF :: ByteString -> PDFWork IO ByteString
optimizeTTF fontData = do
  case ttfParse fontData of
    Left _unableToParse -> return fontData
    Right fontDirectory -> do
      let prepared = fromFontDirectory $ optimizeFontDirectory fontDirectory
      lift $ ttfAutoHintOptimize prepared

{-|
Attempt to optimize a stream, gracefully handling failures.

Extracts the stream from a PDF object, applies the optimization function, and
reports the result. If optimization succeeds, logs a size comparison. If it
fails, logs the error and returns the original unoptimized stream.

This safe wrapper prevents optimization failures from disrupting the overall
optimization process.

__Parameters:__

- A label describing the optimization (e.g., "XML stream optimization")
- The PDF object containing the stream
- A function to apply to the extracted stream

__Returns:__ The optimized stream if successful, or the original stream if
optimization fails.

__Side effects:__ Logs success (with size comparison) or error messages.
-}
optimizeStreamOrIgnore
  :: Logging m
  => T.Text
  -> PDFObject
  -> (ByteString -> PDFWork m ByteString)
  -> PDFWork m ByteString
optimizeStreamOrIgnore optimizationLabel object optimizationProcess = do
  stream <- getStream object
  tryP (optimizationProcess stream) >>= \case
    Right optimizedStream -> do
      sayComparisonP optimizationLabel
                     (BS.length stream)
                     (BS.length optimizedStream)
      return optimizedStream
    Left anError -> do
      sayErrorP "cannot optimize" anError
      return stream

{-|
Apply content-specific stream optimizations to a PDF object.

Determines the optimal optimization strategy based on the object's content type
(XML, graphics, JPEG, TrueType font) and applies the appropriate optimization
process. Updates the object's stream with the optimized data.

__Optimization types:__

- __XML__: Removes unnecessary whitespace and formatting
- __Graphics__: Optimizes PDF graphics commands (scaling, color reduction)
- __JPEG__: Uses jpegtran for lossless JPEG optimization
- __TrueType__: Uses ttfAutoHint for font hinting optimization
- __Other__: Returns the object unchanged

Optimization failures are caught and logged, returning the original stream in
such cases.

__Parameters:__

- A PDF object with stream data

__Returns:__ The object with optimized stream (or unchanged if optimization not
applicable or fails).

__Side effects:__ External processes may be invoked (jpegtran, ttfAutoHint), and
size comparisons are logged.
-}
streamOptimize
  :: Maybe (Dictionary PDFObject)
  -> PDFObject
  -> PDFWork IO PDFObject
streamOptimize resources object = do
  whatOptimizationFor object >>= \case
    XMLOptimization -> do
      optimizedStream <- optimizeStreamOrIgnore "XML stream optimization"
                                                object
                                                optimizeXML
      setStream optimizedStream object

    GfxOptimization -> do
      -- Only a mapped page content stream has the page's initial defaults.
      -- Forms inherit text parameters even when they own their Resources.
      let
        inherited :: Bool
        inherited = isNothing resources
                 || getValueForKey "Subtype" object == Just (PDFName "Form")

      getStream object
        >>= optimizeGFXWithTextState inherited resources
        >>= flip setStream object

    JPGOptimization -> do
      grayObject      <- optimizeJpegColorSpace object
      optimizedStream <- optimizeStreamOrIgnore "JPG stream optimization"
                                                grayObject
                                                (lift . jpegtranOptimize)
      setStream optimizedStream grayObject

    RawBitmapOptimization -> optimizeStreamParity object

    TTFOptimization -> do
      optimizedStream <- optimizeStreamOrIgnore "TTF stream optimization"
                                                object
                                                optimizeTTF
      setStream1 (BS.length optimizedStream) optimizedStream object

    _anyOtherOptimization -> return object

{-|
Completely refilter a stream by finding the best filter combination.

It also optimized nested strings and XML streams.
-}
refilter :: Maybe (Dictionary PDFObject) -> PDFObject -> PDFWork IO PDFObject
refilter resources object = do
  stringOptimized <- deepMapP optimizeString object

  if hasStream object
    then do
      unfiltered <- unfilter stringOptimized >>= restoreOriginalImageEncoding
      optimization <- whatOptimizationFor unfiltered

      streamOptimize resources unfiltered
        >>= filterOptimize optimization
    else
      return stringOptimized

-- | A resized JPEG temporarily lives as lossless samples. Re-encode it with
-- its source quality before selecting JPEG filter candidates, so the optimized
-- resized JPEG remains available alongside JPEG2000. Read the current samples
-- because preceding passes may have zero-filled pixels hidden by a soft mask.
restoreOriginalImageEncoding :: PDFObject -> PDFWork IO PDFObject
restoreOriginalImageEncoding object = do
  bitmaps <- gets wBitmaps
  case getObjectNumber object >>= (`IM.lookup` bitmaps) of
    Just bitmap | JPEGEncoding _ <- bitmapOriginalEncoding bitmap
                , isNothing (getValueForKey "Filter" object) -> do
      samples <- getStream object
      encoded <- lift
               . lift
               $ try
               $ runExceptT (encodeBitmapJPEG bitmap{bitmapSamples = samples})

      case (encoded :: Either IOException (Either UnifiedError ByteString)) of
        Right (Right jpeg) -> do
          sayP "Re-encoding resized JPEG with its source quality"
          setStream jpeg object
            >>= setValue "Filter" (PDFName "DCTDecode")
            >>= removeValue "DecodeParms"

        Right (Left err) -> do
          sayErrorP "Cannot re-encode resized JPEG" err
          return object

        Left _ioError -> do
          sayP "Cannot run JPEG encoder; retaining lossless bitmap samples"
          return object

    _ -> return object

{-|
Check if a PDF filter is known and supported by DietPDF.

Verifies that a filter name is one that DietPDF can reliably decode and
re-encode. Supported filters enable safe optimization; unsupported filters
prevent optimization to avoid data corruption.

__Supported filters:__

- FlateDecode: Standard ZIP compression
- RLEDecode: Run-length encoding
- LZWDecode: LZW compression
- ASCII85Decode: ASCII-85 encoding
- ASCIIHexDecode: Hexadecimal encoding
- DCTDecode: JPEG compression
- JPXDecode: JPEG2000 compression

__Parameters:__

- A PDF filter object

__Returns:__ 'True' if the filter is supported, 'False' otherwise.
-}
isFilterOK :: Filter -> Bool
isFilterOK f = case fFilter f of
  (PDFName "FlateDecode"   ) -> True
  (PDFName "RLEDecode"     ) -> True
  (PDFName "LZWDecode"     ) -> True
  (PDFName "ASCII85Decode" ) -> True
  (PDFName "ASCIIHexDecode") -> True
  (PDFName "DCTDecode"     ) -> True
  (PDFName "JPXDecode"     ) -> True
  _anyOtherCase              -> False

{-|
Determine if a PDF object can be safely optimized.

An object is optimizable if:

- It's an indirect object (always optimizable)
- It's a trailer or cross-reference stream (always optimizable)
- It's a stream object whose filters are all known and supported
- Its structure is inherently optimizable (e.g., dictionary)

Objects with unsupported filters are not optimizable to prevent data corruption.
Simple objects like comments, numbers, and references are not optimizable.

__Parameters:__

- A PDF object

__Returns:__ 'True' if the object can be optimized, 'False' otherwise.

__Side effects:__ May check object's filters, running in the 'PDFWork' monad.
-}
optimizable :: Logging m => PDFObject -> PDFWork m Bool
optimizable PDFIndirectObject{}                  = return True
optimizable PDFTrailer{}                         = return True
optimizable PDFXRefStream{}                      = return True
optimizable object@PDFIndirectObjectWithStream{} = do
  filters <- getFilters object
  let unsupportedFilters = SQ.filter (not . isFilterOK) filters
  return $ SQ.null unsupportedFilters
optimizable object@PDFObjectStream{} = do
  filters <- getFilters object
  let unsupportedFilters = SQ.filter (not . isFilterOK) filters
  return $ SQ.null unsupportedFilters
optimizable _anyOtherObject = return False

{-|
Optimize a PDF object with its resolved graphics resources, when unambiguous.

Nothing disables future resource-dependent passes, but leaves context-free
optimizations available.

`PDFObject` may be optimized by:

- using Zopfli instead of Zlib
- combining Zopfli and RLE
- removing unneeded spaces in XML stream
- optimizing JPG images
- optimizing TTF fonts
- optimizing graphics streams

Optimization of spaces is done at the `PDFObject` level, not by this function.

If the PDF object is not elligible to optimization or if optimization is
ineffective, it is returned as is.
-}
optimize :: Maybe (Dictionary PDFObject) -> PDFObject -> PDFWork IO PDFObject
optimize resources object =
  withContext (ctx ("optimize" :: String) <> ctx object) $ do
    objectCanBeOptimized <- optimizable object

    if objectCanBeOptimized
      then
        tryP (refilter resources object) >>= \case
          Right optimizedObject -> return optimizedObject
          Left  theError        -> do
            sayErrorP "cannot optimize" theError
            return object
      else do
        sayP "ignored"
        return object
