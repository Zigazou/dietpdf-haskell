{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{-|
This module facilitates unfiltering of `PDFObject`.
-}
module Pdf.Object.Unfilter
  ( unfilter
  ) where

import qualified Codec.Compression.Flate       as FL
import qualified Codec.Compression.LZW         as LZ
import qualified Codec.Compression.RunLength   as RL
import qualified Codec.Filter.Ascii85          as A8
import qualified Codec.Filter.AsciiHex         as AH
import qualified Data.ByteString               as BS
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFName
                                                  , PDFIndirectObjectWithStream
                                                  , PDFObjectStream
                                                  )
                                                , Dictionary
                                                )
import           Pdf.Object.Container           ( Filter(fFilter)
                                                , insertMaybes
                                                , setDecodeParms
                                                , setFilters
                                                , getFilters
                                                )
import           Util.Errors                    ( UnifiedError )

unfilterStream
  :: ([Filter], BS.ByteString) -> Either UnifiedError ([Filter], BS.ByteString)
unfilterStream (filters@(pdfFilter : otherFilters), stream)
  | fFilter pdfFilter == PDFName "FlateDecode"
  = FL.decompress stream >>= unfilterStream . (otherFilters, )
  | fFilter pdfFilter == PDFName "RunLengthDecode"
  = RL.decompress stream >>= unfilterStream . (otherFilters, )
  | fFilter pdfFilter == PDFName "LZWDecode"
  = LZ.decompress stream >>= unfilterStream . (otherFilters, )
  | fFilter pdfFilter == PDFName "ASCII85Decode"
  = A8.decode stream >>= unfilterStream . (otherFilters, )
  | fFilter pdfFilter == PDFName "ASCIIHexDecode"
  = AH.decode stream >>= unfilterStream . (otherFilters, )
  | otherwise
  = Right (filters, stream)
unfilterStream (filters, stream) = Right (filters, stream)

unfiltered
  :: Dictionary
  -> BS.ByteString
  -> Either UnifiedError ([Filter], BS.ByteString)
unfiltered dict stream =
  getFilters dict >>= \filters -> unfilterStream (filters, stream)

updateDictionary :: Dictionary -> [Filter] -> Dictionary
updateDictionary dict filters = insertMaybes
  dict
  [("DecodeParms", setDecodeParms filters), ("Filter", setFilters filters)]

unfilter :: PDFObject -> Either UnifiedError PDFObject
unfilter (PDFIndirectObjectWithStream num gen dict stream) = do
  (remainingFilters, unfilteredStream) <- unfiltered dict stream
  return $ PDFIndirectObjectWithStream
    num
    gen
    (updateDictionary dict remainingFilters)
    unfilteredStream
unfilter (PDFObjectStream num gen dict stream) = do
  (remainingFilters, unfilteredStream) <- unfiltered dict stream
  return $ PDFObjectStream num
                           gen
                           (updateDictionary dict remainingFilters)
                           unfilteredStream
unfilter object = return object
