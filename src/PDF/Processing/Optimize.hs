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

import Codec.Compression.Predict (Predictor (PNGOptimum), unpredict)
import Codec.Compression.XML (optimizeXML)

import Control.Monad.State (lift)

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Context (Contextual (ctx))
import Data.Logging (Logging)
import Data.PDF.Filter (Filter (fFilter))
import Data.PDF.OptimizationType
  ( OptimizationType (GfxOptimization, JPGOptimization, RawBitmapOptimization, TTFOptimization, XMLOptimization)
  )
import Data.PDF.PDFObject
  ( PDFObject (PDFIndirectObject, PDFIndirectObjectWithStream, PDFName, PDFObjectStream, PDFTrailer, PDFXRefStream)
  , hasStream
  )
import Data.PDF.PDFWork
  (PDFWork, sayComparisonP, sayErrorP, sayP, tryP, withContext)
import Data.Sequence qualified as SQ
import Data.Text qualified as T

import External.JpegTran (jpegtranOptimize)
import External.TtfAutoHint (ttfAutoHintOptimize)

import PDF.Graphics.Optimize (optimizeGFX)
import PDF.Object.Container (getFilters)
import PDF.Object.Object (PDFObject (PDFNumber))
import PDF.Object.State (getStream, getValue, setStream, setStream1, setValue)
import PDF.Object.String (optimizeString)
import PDF.Processing.Filter (filterOptimize)
import PDF.Processing.PDFWork (deepMapP)
import PDF.Processing.Unfilter (unfilter)
import PDF.Processing.WhatOptimizationFor (whatOptimizationFor)

import Util.ByteString (containsOnlyGray, convertToGray, optimizeParity)

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
getWidthComponents :: Logging m => PDFObject -> PDFWork m (Maybe (Int, Int))
getWidthComponents object@(PDFIndirectObjectWithStream _number _version _dict _stream) = do
  mWidth      <- getValue "Width" object
  mColorSpace <- getValue "ColorSpace" object

  let components :: Int
      components = case mColorSpace of
        Just (PDFName "DeviceRGB" ) -> 3
        Just (PDFName "DeviceCMYK") -> 4
        _anyOtherValue              -> 1

  return $ case mWidth of
    Just (PDFNumber width) -> Just (round width, components)
    _anyOtherValue         -> Nothing

getWidthComponents _anyOtherObject = return Nothing

{-|
Optimize bitmap stream data by aligning color components and reducing color space.

Performs intelligent optimization of raw bitmap streams by:

1. __Unpredicting__: Reverses PNG prediction if width and components are known
2. __Grayscale conversion__: Detects RGB images containing only gray values and
   converts them to DeviceGray color space, reducing data size by 66%
3. __Parity optimization__: Aligns color component bytes for better compression
   in both RGB and other color spaces

The function safely handles unprediction failures by falling back to the
original stream data.

__Optimization strategies:__

- __DeviceRGB__: Check if image is actually grayscale; if so, convert to
  DeviceGray and update color space. Otherwise, optimize RGB parity.
- __Other color spaces__: Optimize parity if stream length is divisible by 3
  (suggesting triplet structure).

__Parameters:__

- A PDF object containing bitmap stream data

__Returns:__ The object with optimized stream and potentially updated color space
dictionary entry.

__Side effects:__ Logs optimization actions ("Gray bitmap optimization", "RGB
parity optimization", "Parity optimization").
-}
optimizeStreamParity :: PDFObject -> PDFWork IO PDFObject
optimizeStreamParity object = do
  mWidthComponents <- getWidthComponents object

  stream <- getStream object

  -- Unpredict the stream if needed
  let rawStream = case mWidthComponents of
                    Just (width, components) ->
                      case unpredict PNGOptimum width components stream of
                        (Right predicted)  -> predicted
                        (Left  _err      ) -> stream
                    Nothing -> stream

  mColorSpace <- getValue "ColorSpace" object
  if mColorSpace == Just (PDFName "DeviceRGB")
    then
      if containsOnlyGray rawStream
        then do
          optimizedStream <- optimizeStreamOrIgnore "Gray bitmap optimization"
                                                    object
                                                    (return . convertToGray)
          setValue "ColorSpace" (PDFName "DeviceGray") object
            >>= setStream optimizedStream
        else do
          let optimizedStream = optimizeParity rawStream
          sayP "RGB parity optimization"
          setStream optimizedStream object
    else do
      if BS.length rawStream `mod` 3 == 0
        then do
          let optimizedStream = optimizeParity rawStream
          sayP "Parity optimization"
          setStream optimizedStream object
        else
          return object

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
streamOptimize :: PDFObject -> PDFWork IO PDFObject
streamOptimize object = do
  whatOptimizationFor object >>= \case
    XMLOptimization -> do
      optimizedStream <- optimizeStreamOrIgnore "XML stream optimization"
                                                object
                                                optimizeXML
      setStream optimizedStream object

    GfxOptimization -> do
      getStream object >>= optimizeGFX >>= flip setStream object

    JPGOptimization -> do
      optimizedStream <- optimizeStreamOrIgnore "JPG stream optimization"
                                                object
                                                (lift . jpegtranOptimize)
      setStream optimizedStream object

    RawBitmapOptimization -> optimizeStreamParity object

    TTFOptimization -> do
      optimizedStream <- optimizeStreamOrIgnore "TTF stream optimization"
                                                object
                                                (lift . ttfAutoHintOptimize)
      setStream1 (BS.length optimizedStream) optimizedStream object

    _anyOtherOptimization -> return object

{-|
Completely refilter a stream by finding the best filter combination.

It also optimized nested strings and XML streams.
-}
refilter :: PDFObject -> PDFWork IO PDFObject
refilter object = do
  stringOptimized <- deepMapP optimizeString object

  if hasStream object
    then do
      optimization <- whatOptimizationFor object
      unfilter stringOptimized
        >>= streamOptimize
        >>= filterOptimize optimization
    else return stringOptimized

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
Optimize a PDF object.

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
optimize :: PDFObject -> PDFWork IO PDFObject
optimize object = withContext (ctx ("optimize" :: String) <> ctx object) $ do
  objectCanBeOptimized <- optimizable object

  if objectCanBeOptimized
    then
      -- refilter object
      tryP (refilter object) >>= \case
       Right optimizedObject -> return optimizedObject
       Left  theError         -> do
         sayErrorP "cannot optimize" theError
         return object
    else do
      sayP "ignored"
      return object
