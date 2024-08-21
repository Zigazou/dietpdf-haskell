{-|
This module facilitates unfiltering of `PDFObject`.
-}
module Pdf.Object.Unfilter
  ( unfilter
  ) where

import Codec.Compression.Flate qualified as FL
import Codec.Compression.LZW qualified as LZ
import Codec.Compression.RunLength qualified as RL
import Codec.Filter.Ascii85 qualified as A8
import Codec.Filter.AsciiHex qualified as AH

import Control.Monad.Trans.Except (throwE)

import Data.ByteString qualified as BS
import Data.Sequence as SQ (Seq ((:<|)))
import Data.Text qualified as T

import Pdf.Object.Container
    ( Filter (fFilter)
    , FilterList
    , getFilters
    , setFilters
    )
import Pdf.Object.Format (txtObjectNumberVersion)
import Pdf.Object.Object (PDFObject (PDFName), hasStream)
import Pdf.Object.State (getStream, setStream)

import Util.Logging (Logging, sayF)
import Util.UnifiedError (FallibleT, UnifiedError)

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
