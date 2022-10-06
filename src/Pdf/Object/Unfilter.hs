{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

{-|
This module facilitates unfiltering of `PDFObject`.
-}
module Pdf.Object.Unfilter
  ( unfilter
  , supportedFilters
  ) where

import qualified Codec.Compression.Flate       as FL
import qualified Codec.Compression.LZW         as LZ
import qualified Codec.Compression.RunLength   as RL
import qualified Codec.Filter.Ascii85          as A8
import qualified Codec.Filter.AsciiHex         as AH
import           Control.Monad.State            ( StateT
                                                , get
                                                , lift
                                                )
import qualified Data.ByteString               as BS
import           Pdf.Object.Container           ( Filter(fFilter)
                                                , getFilters
                                                , setFilters
                                                )
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFIndirectObjectWithStream
                                                  , PDFName
                                                  , PDFObjectStream
                                                  )
                                                , getStream
                                                , setStream
                                                )
import           Util.Errors                    ( UnifiedError )

supportedFilters :: [BS.ByteString]
supportedFilters =
  [ "FlateDecode"
  , "RunLengthDecode"
  , "LZWDecode"
  , "ASCII85Decode"
  , "ASCIIHexDecode"
  ]

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

unfiltered :: StateT PDFObject (Either UnifiedError) ([Filter], BS.ByteString)
unfiltered = do
  stream <- getStream
  getFilters >>= \filters -> lift (unfilterStream (filters, stream))

unfilter :: StateT PDFObject (Either UnifiedError) ()
unfilter = do
  get >>= \case
    PDFIndirectObjectWithStream{} -> unfilter'
    PDFObjectStream{}             -> unfilter'
    _anyOtherObject               -> return ()
 where
  unfilter' :: StateT PDFObject (Either UnifiedError) ()
  unfilter' = do
    (remainingFilters, unfilteredStream) <- unfiltered
    setStream unfilteredStream
    setFilters remainingFilters
