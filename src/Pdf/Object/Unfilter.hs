{-|
This module facilitates unfiltering of `PDFObject`.
-}
module Pdf.Object.Unfilter
  ( unfilter
  ) where

import Codec.Compression.Flate qualified as FL
import Codec.Compression.LZW qualified as LZ
import Codec.Compression.Predictor (Predictor, toPredictor, unpredict)
import Codec.Compression.RunLength qualified as RL
import Codec.Filter.Ascii85 qualified as A8
import Codec.Filter.AsciiHex qualified as AH

import Control.Monad.Trans.Except (runExcept, throwE)

import Data.ByteString qualified as BS
import Data.Sequence as SQ (Seq ((:<|)))
import Data.Text qualified as T

import Pdf.Object.Container
    ( Filter (fDecodeParms, fFilter)
    , FilterList
    , getFilters
    , setFilters
    )
import Pdf.Object.Format (txtObjectNumberVersion)
import Pdf.Object.Object (PDFObject (PDFName, PDFNumber), hasKey, hasStream)
import Pdf.Object.State (getStream, getValue, getValueDefault, setStream)

import Util.Logging (Logging, sayF)
import Util.UnifiedError (FallibleT, UnifiedError (InvalidFilterParm))

getPredictor :: PDFObject -> Either UnifiedError Predictor
getPredictor params = case runExcept (getValue "Predictor" params) of
  Right (Just (PDFNumber value)) -> toPredictor . round $ value
  _anythingElse                  -> Left InvalidFilterParm

getColumns :: PDFObject -> Either UnifiedError Int
getColumns params = case runExcept (getValueDefault "Columns" (PDFNumber 1) params) of
  Right (Just (PDFNumber value)) -> return . round $ value
  _anythingElse                  -> Left InvalidFilterParm

getComponents :: PDFObject -> Either UnifiedError Int
getComponents params = case runExcept (getValueDefault "BitsPerComponent" (PDFNumber 8) params) of
  Right (Just (PDFNumber value)) -> return . round $ value
  _anythingElse                  -> Left InvalidFilterParm

unpredictStream :: Filter -> BS.ByteString -> Either UnifiedError BS.ByteString
unpredictStream pdfFilter stream =
  let params = fDecodeParms pdfFilter in
  if hasKey "Predictor" params
  then do
    predictor <- getPredictor params
    columns <- getColumns params
    components <- getComponents params
    unpredict predictor columns (components `div` 8) stream
  else return stream

unfilterStream
  :: (FilterList, BS.ByteString)
  -> Either UnifiedError (FilterList, BS.ByteString)
unfilterStream (filters@(pdfFilter :<| otherFilters), stream)
  | fFilter pdfFilter == PDFName "FlateDecode"
  = FL.decompress stream >>= unpredictStream pdfFilter >>= unfilterStream . (otherFilters, )
  | fFilter pdfFilter == PDFName "RunLengthDecode"
  = RL.decompress stream >>= unfilterStream . (otherFilters, )
  | fFilter pdfFilter == PDFName "LZWDecode"
  = LZ.decompress stream >>= unpredictStream pdfFilter  >>= unfilterStream . (otherFilters, )
  | fFilter pdfFilter == PDFName "ASCII85Decode"
  = A8.decode stream >>= unfilterStream . (otherFilters, )
  | fFilter pdfFilter == PDFName "ASCIIHexDecode"
  = AH.decode stream >>= unfilterStream . (otherFilters, )
  | otherwise
  = Right (filters, stream)
unfilterStream (filters, stream) = Right (filters, stream)

unfiltered :: Logging m => PDFObject -> FallibleT m (FilterList, BS.ByteString)
unfiltered object = do
  stream  <- getStream object
  filters <- getFilters object
  case unfilterStream (filters, stream) of
    Right unfilteredData  -> return unfilteredData
    Left  unfilteredError -> throwE unfilteredError

{- |
Tries to decode every filter of an object with a stream.

It usually decompresses the stream.
-}
unfilter :: Logging m => PDFObject -> FallibleT m PDFObject
unfilter object = if hasStream object
  then do
    sayF (T.concat ["  - Unfiltering ", txtObjectNumberVersion object])
    unfiltered object >>= \(remainingFilters, unfilteredStream) ->
      setStream unfilteredStream object >>= setFilters remainingFilters
  else return object
