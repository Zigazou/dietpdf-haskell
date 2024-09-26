{-|
This module facilitates unfiltering of `PDFObject`.
-}
module Pdf.Object.Unfilter
  ( unfilter
  ) where

import Codec.Compression.Flate qualified as FL
import Codec.Compression.LZW qualified as LZ
import Codec.Compression.Predict (Predictor, unpredict)
import Codec.Compression.Predict.Predictor (decodePredictor)
import Codec.Compression.RunLength qualified as RL
import Codec.Filter.Ascii85 qualified as A8
import Codec.Filter.AsciiHex qualified as AH

import Control.Monad.Trans.Except (runExcept, throwE)

import Data.ByteString qualified as BS
import Data.Logging (Logging)
import Data.Sequence as SQ (Seq ((:<|)))
import Data.UnifiedError (FallibleT, UnifiedError (InvalidFilterParm))

import Pdf.Object.Container
    ( Filter (fDecodeParms, fFilter)
    , FilterList
    , getFilters
    , setFilters
    )
import Pdf.Object.Object
    ( PDFObject (PDFName, PDFNumber)
    , ToPDFNumber (mkPDFNumber)
    , hasKey
    , hasStream
    )
import Pdf.Object.State (getStream, getValue, getValueDefault, setStream)

getPredictor :: PDFObject -> Either UnifiedError Predictor
getPredictor params = case runExcept (getValue "Predictor" params) of
  Right (Just (PDFNumber value)) -> decodePredictor . round $ value
  _anythingElse                  -> Left InvalidFilterParm

getColumns :: PDFObject -> Either UnifiedError Int
getColumns params = case runExcept (getValueDefault "Columns" (PDFNumber 1) params) of
  Right (Just (PDFNumber value)) -> return . round $ value
  _anythingElse                  -> Left InvalidFilterParm

getComponents :: PDFObject -> Either UnifiedError Int
getComponents params = case runExcept (getValueDefault "BitsPerComponent" (PDFNumber 8) params) of
  Right (Just (PDFNumber value)) -> return . round $ value
  _anythingElse                  -> Left InvalidFilterParm

getColors :: Int -> PDFObject -> Either UnifiedError Int
getColors defaultColors params = case runExcept (getValueDefault "Colors" (mkPDFNumber defaultColors) params) of
  Right (Just (PDFNumber value)) -> return . round $ value
  _anythingElse                  -> Left InvalidFilterParm

unpredictStream :: Int -> Filter -> BS.ByteString -> Either UnifiedError BS.ByteString
unpredictStream defaultColors pdfFilter stream =
  let params = fDecodeParms pdfFilter in
  if hasKey "Predictor" params
  then do
    predictor  <- getPredictor params
    columns    <- getColumns params
    components <- getComponents params
    colors     <- getColors defaultColors params
    unpredict predictor columns (colors * components `div` 8) stream
  else return stream

unfilterStream
  :: Int
  -> (FilterList, BS.ByteString)
  -> Either UnifiedError (FilterList, BS.ByteString)
unfilterStream colors (filters@(pdfFilter :<| otherFilters), stream)
  | fFilter pdfFilter == PDFName "FlateDecode"
  = FL.decompress stream >>= unpredictStream colors pdfFilter
                         >>= unfilterStream colors . (otherFilters, )
  | fFilter pdfFilter == PDFName "RunLengthDecode"
  = RL.decompress stream >>= unfilterStream colors . (otherFilters, )
  | fFilter pdfFilter == PDFName "LZWDecode"
  = LZ.decompress stream >>= unpredictStream colors pdfFilter
                         >>= unfilterStream colors . (otherFilters, )
  | fFilter pdfFilter == PDFName "ASCII85Decode"
  = A8.decode stream >>= unfilterStream colors . (otherFilters, )
  | fFilter pdfFilter == PDFName "ASCIIHexDecode"
  = AH.decode stream >>= unfilterStream colors . (otherFilters, )
  | otherwise
  = Right (filters, stream)
unfilterStream _colors (filters, stream) = Right (filters, stream)

getColorSpace :: Logging m => PDFObject -> FallibleT m Int
getColorSpace params = case runExcept (getValueDefault "ColorSpace" (PDFName "DeviceRGB") params) of
  Right (Just (PDFName "DeviceGray")) -> return 1
  Right (Just (PDFName "DeviceRGB"))  -> return 3
  Right (Just (PDFName "DeviceCMYK")) -> return 4
  Right _anyOtherColorSpace           -> return 1
  _anythingElse                       -> throwE InvalidFilterParm

unfiltered :: Logging m => PDFObject -> FallibleT m (FilterList, BS.ByteString)
unfiltered object = do
  stream  <- getStream object
  filters <- getFilters object
  colorSpace <- getColorSpace object
  case unfilterStream colorSpace (filters, stream) of
    Right unfilteredData  -> return unfilteredData
    Left  unfilteredError -> throwE unfilteredError

{- |
Tries to decode every filter of an object with a stream. Some filters will not
be decoded because (like `DCTDecode`).

It usually decompresses the stream.
-}
unfilter :: Logging m => PDFObject -> FallibleT m PDFObject
unfilter object = if hasStream object
  then
    unfiltered object >>= \(remainingFilters, unfilteredStream) ->
      setStream unfilteredStream object >>= setFilters remainingFilters
  else return object
