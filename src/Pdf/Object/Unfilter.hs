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
import qualified Data.ByteString               as BS
import           Data.Sequence                 as SQ
                                                ( Seq((:<|)) )
import           Pdf.Object.Container           ( Filter(fFilter)
                                                , getFilters
                                                , setFilters
                                                , FilterList
                                                )
import           Pdf.Object.Object              ( PDFObject(PDFName)
                                                , hasStream
                                                )
import           Pdf.Object.State               ( getStream
                                                , setStream
                                                )
import           Util.UnifiedError              ( UnifiedError
                                                , FallibleT
                                                )
import           Util.Logging                   ( Logging
                                                , sayF
                                                )
import           Control.Monad.Trans.Except     ( throwE )
import           Pdf.Object.Format              ( txtObjectNumberVersion )
import qualified Data.Text                     as T

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
