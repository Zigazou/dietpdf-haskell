{-|
Unfiltering of PDF object streams.

This module attempts to decode supported filters from object streams
(`FlateDecode`, `RunLengthDecode`, `LZWDecode`, `ASCII85Decode`,
`ASCIIHexDecode`) and applies PNG-style predictors when specified. It preserves
unsupported filters and returns any remaining filter list alongside the
unfiltered stream.
-}
module PDF.Processing.Unfilter
  ( unfilter
  ) where

import Codec.Compression.Flate qualified as FL
import Codec.Compression.LZW qualified as LZ
import Codec.Compression.Predict (Predictor, unpredict)
import Codec.Compression.Predict.Predictor (decodePredictor)
import Codec.Compression.RunLength qualified as RL
import Codec.Filter.Ascii85 qualified as A8
import Codec.Filter.AsciiHex qualified as AH

import Data.Bitmap.BitmapConfiguration
  ( BitmapConfiguration (BitmapConfiguration, bcBitsPerComponent, bcComponents, bcLineWidth)
  )
import Data.Bitmap.BitsPerComponent (BitsPerComponent)
import Data.ByteString (ByteString)
import Data.Logging (Logging)
import Data.PDF.Filter (Filter (fDecodeParms, fFilter))
import Data.PDF.FilterList (FilterList)
import Data.PDF.PDFObject (PDFObject, hasStream)
import Data.PDF.PDFWork (PDFWork, fallibleP, throwError, tryP)
import Data.Sequence as SQ (Seq ((:<|)))
import Data.UnifiedError (UnifiedError (InvalidFilterParm))

import PDF.Object.Container (getFilters, setFilters)
import PDF.Object.Object
  (PDFObject (PDFName, PDFNumber), ToPDFNumber (mkPDFNumber), hasKey)
import PDF.Object.State (getStream, getValue, getValueDefault, setStream)

{-|
Extract and decode the `Predictor` value from filter parameters. Throws
`InvalidFilterParm` when the predictor is missing or invalid.
-}
getPredictor :: Logging m => PDFObject -> PDFWork m Predictor
getPredictor params =
  tryP (getValue "Predictor" params) >>= \case
    Right (Just (PDFNumber value)) -> case decodePredictor . round $ value of
      Right predictor -> return predictor
      _anythingElse -> throwError
        $ InvalidFilterParm ("Invalid predictor=" ++ show value)
    _anythingElse -> throwError
      $ InvalidFilterParm ("Missing predictor for params=" ++ show params)

{-|
Read the `Columns` parameter; defaults to 1. Throws `InvalidFilterParm` on
invalid values.
-}
getColumns :: Logging m => PDFObject -> PDFWork m Int
getColumns params =
  tryP (getValueDefault "Columns" (PDFNumber 1) params) >>= \case
    Right (Just (PDFNumber value)) -> return . round $ value
    _anythingElse -> throwError
      $ InvalidFilterParm ("Columns=" ++ show params)

{-|
Read `BitsPerComponent`; defaults to 8.
Throws `InvalidFilterParm` on invalid values.
-}
getComponents :: Logging m => PDFObject -> PDFWork m BitsPerComponent
getComponents params =
  tryP (getValueDefault "BitsPerComponent" (PDFNumber 8) params) >>= \case
    Right (Just (PDFNumber value)) -> return $ (toEnum . round) value
    _anythingElse -> throwError
      $ InvalidFilterParm ("BitsPerComponent=" ++ show params)

{-|
Read `Colors` with a provided default; returns the color count.
Throws `InvalidFilterParm` on invalid values.
-}
getColors :: Logging m => Int -> PDFObject -> PDFWork m Int
getColors defaultColors params =
  tryP (getValueDefault "Colors" (mkPDFNumber defaultColors) params) >>= \case
    Right (Just (PDFNumber value)) -> return . round $ value
    _anythingElse                  -> throwError
      $ InvalidFilterParm ("Colors=" ++ show params)

{-|
Apply PNG predictor decoding when `Predictor` is present in filter parameters;
otherwise return the stream unchanged.
-}
unpredictStream
  :: Logging m
  => Int
  -> Filter
  -> ByteString
  -> PDFWork m ByteString
unpredictStream defaultColors pdfFilter stream =
  let params = fDecodeParms pdfFilter
  in if not (hasKey "Predictor" params)
    then return stream
    else do
      predictor  <- getPredictor params
      columns    <- getColumns params
      components <- getComponents params
      colors     <- getColors defaultColors params

      let bitmapConfig = BitmapConfiguration
            { bcLineWidth        = columns
            , bcComponents       = colors
            , bcBitsPerComponent = components
            }

      case unpredict predictor bitmapConfig stream of
        Right unpredictedStream -> return unpredictedStream
        Left unpredictError     -> throwError unpredictError

{-|
Decode supported filters in order, threading remaining filters forward. For
`FlateDecode` and `LZWDecode`, applies predictor decoding when appropriate.
-}
unfilterStream
  :: Logging m
  => Int
  -> (FilterList, ByteString)
  -> PDFWork m (FilterList, ByteString)
unfilterStream colors (filters@(pdfFilter :<| otherFilters), stream)
  | fFilter pdfFilter == PDFName "FlateDecode" =
          fallibleP (FL.decompress stream)
      >>= unpredictStream colors pdfFilter
      >>= unfilterStream colors . (otherFilters, )
  | fFilter pdfFilter == PDFName "RunLengthDecode" =
          fallibleP (RL.decompress stream)
      >>= unfilterStream colors . (otherFilters, )
  | fFilter pdfFilter == PDFName "LZWDecode" =
          fallibleP (LZ.decompress stream)
      >>= unpredictStream colors pdfFilter
      >>= unfilterStream colors . (otherFilters, )
  | fFilter pdfFilter == PDFName "ASCII85Decode" =
          fallibleP (A8.decode stream)
      >>= unfilterStream colors . (otherFilters, )
  | fFilter pdfFilter == PDFName "ASCIIHexDecode" =
          fallibleP (AH.decode stream)
      >>= unfilterStream colors . (otherFilters, )
  | otherwise = return (filters, stream)
unfilterStream _colors (filters, stream) = return (filters, stream)

{-|
Infer a color-space channel count from the `ColorSpace` key.
Defaults to 1 channel when the value is unrecognized.
-}
getColorSpace :: Logging m => PDFObject -> PDFWork m Int
getColorSpace params =
  tryP (getValueDefault "ColorSpace" (PDFName "DeviceGray") params) >>= \case
    Right (Just (PDFName "DeviceGray")) -> return 1
    Right (Just (PDFName "DeviceRGB"))  -> return 3
    Right (Just (PDFName "DeviceCMYK")) -> return 4
    Right _anyOtherColorSpace           -> return 1
    _anythingElse -> throwError
      $ InvalidFilterParm ("ColorSpace=" ++ show params)

{-|
Retrieve the stream and filter list from an object, determine the
color-space, and decode filters, producing the remaining filters and the
unfiltered stream.
-}
unfiltered :: Logging m => PDFObject -> PDFWork m (FilterList, ByteString)
unfiltered object = do
  stream     <- getStream object
  filters    <- getFilters object
  colorSpace <- getColorSpace object
  unfilterStream colorSpace (filters, stream)

{-|
Tries to decode every filter of an object with a stream. Some filters will not
be decoded because (like `DCTDecode`).

It usually decompresses the stream.
-}
unfilter :: Logging m => PDFObject -> PDFWork m PDFObject
unfilter object = if not (hasStream object)
  then return object
  else do
    unfiltered object
      >>= \(remainingFilters, unfilteredStream) ->
          setStream unfilteredStream object
      >>= setFilters remainingFilters
