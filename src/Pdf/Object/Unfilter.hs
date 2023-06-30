{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

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
import           Control.Monad.State            ( lift )
import qualified Data.ByteString               as BS
import           Data.Sequence                 as SQ
                                                ( Seq((:<|)) )
import           Pdf.Object.Container           ( Filter(fFilter)
                                                , getFilters
                                                , setFilters
                                                , FilterList
                                                )
import           Pdf.Object.Object              ( PDFObject(PDFName) )
import           Pdf.Object.State               ( getStream
                                                , setStream
                                                , FallibleComputation
                                                , ifObject
                                                , hasStreamS
                                                )
import           Util.Errors                    ( UnifiedError )

{- |
List of all filters that the `unfilter` function supports.
-}
supportedFilters :: [BS.ByteString]
supportedFilters =
  [ "FlateDecode"
  , "RunLengthDecode"
  , "LZWDecode"
  , "ASCII85Decode"
  , "ASCIIHexDecode"
  ]

unfilterStream
  :: (FilterList, BS.ByteString)
  -> Either UnifiedError (FilterList, BS.ByteString)
unfilterStream (filters@(pdfFilter :<| otherFilters), stream)
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

unfiltered :: FallibleComputation (FilterList, BS.ByteString)
unfiltered = do
  stream  <- getStream
  filters <- getFilters
  lift (unfilterStream (filters, stream))

{- |
Tries to decode every filter of an object with a stream.

It usually decompresses the stream.
-}
unfilter :: FallibleComputation ()
unfilter = ifObject hasStreamS $ do
  (remainingFilters, unfilteredStream) <- unfiltered
  setStream unfilteredStream
  setFilters remainingFilters
