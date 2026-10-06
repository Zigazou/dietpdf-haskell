-- | Decode, resize and losslessly store supported non-indexed image samples.
module PDF.Processing.ResizeImage (resizeImage) where

import Codec.Compression.Flate qualified as Flate

import Control.Exception (IOException, try)
import Control.Monad (guard)
import Control.Monad.State (gets, lift, modify)
import Control.Monad.Trans.Except (runExceptT)

import Data.Bitmap.Bitmap
  ( Bitmap (Bitmap, bitmapOriginalEncoding)
  , BitmapEncoding (JPEG2000Encoding, JPEGEncoding, RawEncoding)
  )
import Data.Bitmap.BitmapConfiguration
  (BitmapConfiguration (BitmapConfiguration))
import Data.Bitmap.BitsPerComponent (BitsPerComponent (BC16Bits, BC8Bits))
import Data.Bitmap.Resize (resizeBitmap)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.PDF.PDFObject
  ( PDFObject (PDFArray, PDFBool, PDFIndirectObjectWithStream, PDFName, PDFNumber)
  )
import Data.PDF.PDFWork (PDFWork, sayP, tryP)
import Data.PDF.WorkData (wBitmaps)
import Data.UnifiedError (UnifiedError)

import External.ExternalCommand (externalCommandBuf)
import External.ImageMagick (jpegQualityHint)

import PDF.Document.ResourceContext (resolve, value)
import PDF.Graphics.Visibility (finite)
import PDF.Object.Object.Properties (getValueForKey)
import PDF.Processing.Unfilter (unfilter)

import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)

{- | Resize only 8- or 16-bit non-indexed images. Raw/predictor samples keep
their color space, Decode array and bit depth. JPEG/JPX decoding uses
ImageMagick for DeviceGray/DeviceRGB; the replacement is lossless Flate.
Unsupported encodings or metadata retain the original object. Masks themselves
are excluded by the document pass; external soft/stencil masks retain their own
sampling grid. Color-key masks and embedded alpha cannot safely be averaged.
-}
resizeImage
  :: IntMap PDFObject
  -> (Int, Int)
  -> PDFObject
  -> PDFWork IO PDFObject
resizeImage objects
           (limitWidth, limitHeight)
           original@(PDFIndirectObjectWithStream number generation dictionary _)
  = case metadata of
      Nothing -> return original

      Just (width, height, bits, components, colorSpace) -> do
        let
          targetWidth :: Int
          targetWidth = min width (max 1 limitWidth)

          targetHeight :: Int
          targetHeight = min height (max 1 limitHeight)

        if (targetWidth, targetHeight) == (width, height)
          then
            return original
          else do
            -- Resolving the color space also gives unfilter the channel count
            -- for predictor defaults when it is an indirect device color space.
            let
              normalized :: PDFObject
              normalized = PDFIndirectObjectWithStream
                            number
                            generation
                            (Map.insert "ColorSpace" colorSpace dictionary)
                            (streamOf original)

            decoded <- tryP (unfilter normalized)
            knownBitmaps <- gets wBitmaps

            samples <- case decoded of
              Right (PDFIndirectObjectWithStream _ _ decodedDictionary bytes) ->
                case Map.lookup "Filter" decodedDictionary of
                  Nothing
                    -> return (Just ( bytes
                                    , maybe RawEncoding
                                            bitmapOriginalEncoding
                                            (IM.lookup number knownBitmaps)
                                    )
                              )

                  Just (PDFName "DCTDecode")
                    | bits == 8
                    && Map.notMember "DecodeParms" decodedDictionary ->
                        do
                          quality <- lift
                                   . lift
                                   $ try
                                   $ runExceptT (jpegQualityHint bytes)

                          case (quality :: Either
                                            IOException
                                            (Either UnifiedError (Maybe Int))
                               ) of
                            Right (Right (Just q)) ->
                              fmap (fmap (, JPEGEncoding q))
                                   (decodeImage "jpg" bits colorSpace bytes)

                            _ -> return Nothing

                  Just (PDFName "JPXDecode")
                    | Map.notMember "DecodeParms" decodedDictionary ->
                        fmap (fmap (, JPEG2000Encoding))
                             (decodeImage "jp2" bits colorSpace bytes)

                  _ -> return Nothing

              _ -> return Nothing

            let
              resizedSamples :: Maybe (ByteString, BitmapEncoding)
              resizedSamples = do
                (pixels, encoding) <- samples
                raw <- resizeBitmap width
                                    height
                                    components
                                    bits
                                    targetWidth
                                    targetHeight
                                    pixels

                return (raw, encoding)

            case resizedSamples of
              Just (raw, encoding) | Right compressed <- Flate.fastCompress raw
                -> do
                  sayP "Downsampling image to the requested DPI"
                  let
                    bitmap :: Bitmap
                    bitmap = Bitmap
                        (BitmapConfiguration
                          targetWidth
                          components
                          (if bits == 8 then BC8Bits else BC16Bits)
                        )
                        targetHeight
                        raw
                        encoding

                  modify (\state ->
                            state { wBitmaps = IM.insert number
                                                         bitmap
                                                         (wBitmaps state)
                                  }
                         )

                  let
                    updated :: Map.Map ByteString PDFObject
                    updated = Map.union
                      (Map.fromList
                        [ ( "Width"
                          , PDFNumber (fromIntegral targetWidth)
                          )
                        , ( "Height"
                          , PDFNumber (fromIntegral targetHeight)
                          )
                        , ( "Length"
                          , PDFNumber (fromIntegral (BS.length compressed))
                          )
                        , ( "Filter"
                          , PDFName "FlateDecode"
                          )
                        ]
                      )
                      (Map.delete "DecodeParms" dictionary)

                  return (PDFIndirectObjectWithStream number
                                                      generation
                                                      updated
                                                      compressed
                        )

              _ -> do
                sayP "Cannot decode or resize image; retaining original \
                     \resolution"
                return original
 where
  metadata :: Maybe (Int, Int, Int, Int, PDFObject)
  metadata = do
    guard (value objects "Subtype" original == Just (PDFName "Image"))
    guard (value objects "ImageMask" original /= Just (PDFBool True))
    guard (isNothing (getValueForKey "SMaskInData" original))
    guard (isNothing (getValueForKey "Alternates" original))
    guard (isNothing (getValueForKey "F" original))

    case value objects "SMask" original of
      Just mask -> guard (isNothing (getValueForKey "Matte" mask))
      Nothing   -> Just ()

    case value objects "Mask" original of
      Just PDFArray{} -> Nothing
      _               -> Just ()

    width <- value objects "Width" original >>= positiveInteger
    height <- value objects "Height" original >>= positiveInteger
    bits <- value objects "BitsPerComponent" original >>= positiveInteger

    guard (bits == 8 || bits == 16)

    colorSpace <- value objects "ColorSpace" original
    components <- componentCount objects colorSpace

    return (width, height, bits, components, colorSpace)

resizeImage _ _ original = return original

streamOf :: PDFObject -> ByteString
streamOf (PDFIndirectObjectWithStream _ _ _ bytes) = bytes
streamOf _                                         = BS.empty

positiveInteger :: PDFObject -> Maybe Int
positiveInteger (PDFNumber n) = do
  guard (finite n && n > 0 && n < fromIntegral (maxBound :: Int))
  guard (n == fromInteger (round n))
  return (round n)
positiveInteger _ = Nothing

componentCount :: IntMap PDFObject -> PDFObject -> Maybe Int
componentCount _ (PDFName "DeviceGray") = Just 1

componentCount _ (PDFName "DeviceRGB") = Just 3

componentCount _ (PDFName "DeviceCMYK") = Just 4

componentCount objects (PDFArray entries) = case foldr (:) [] entries of
  [PDFName "CalGray", _]
    -> Just 1

  [PDFName "CalRGB", _]
    -> Just 3

  [PDFName "Lab", _]
    -> Just 3

  [PDFName "ICCBased", profile]
    -> resolve objects profile >>= value objects "N" >>= positiveInteger

  [PDFName "Separation", _, _, _]
    -> Just 1

  PDFName "DeviceN" : PDFArray names : _
    -> Just (length names)

  _
    -> Nothing

componentCount _ _ = Nothing

-- | Headered codecs are decoded through a temporary input file so large images
-- cannot deadlock a process while writing stdin and reading stdout
-- sequentially.
decodeImage
  :: String
  -> Int
  -> PDFObject
  -> ByteString
  -> PDFWork IO (Maybe ByteString)
decodeImage format bits colorSpace bytes = case colorSpace of
  PDFName "DeviceGray" -> decode "gray"

  PDFName "DeviceRGB"  -> decode "rgb"

  -- JPEG CMYK decoders may implicitly invert Adobe samples. Retaining the PDF
  -- Decode array would then apply the inversion twice. Raw CMYK resizing
  -- remains supported; headered CMYK codecs require a PDF-aware decoder.
  PDFName "DeviceCMYK" -> return Nothing

  _                    -> return Nothing
 where
  decode :: String -> PDFWork IO (Maybe ByteString)
  decode rawFormat = do
    result <- lift . lift $ try $ runExceptT $
      withSystemTempFile ("dietpdf-dpi." ++ format) $ \path handle -> do
        lift (hClose handle)
        lift (BS.writeFile path bytes)

        externalCommandBuf "convert"
          [ format ++ ":" ++ path
          , "-depth", show bits
          , "-endian", "MSB"
          , "-define", "quantum:format=unsigned"
          , rawFormat ++ ":-"
          ] BS.empty

    return $
      case (result :: Either IOException (Either UnifiedError ByteString)) of
        Right (Right raw) -> Just raw
        _                 -> Nothing
