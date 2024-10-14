module PDF.Processing.FilterCombine.RleZopfli
  ( rleZopfli
  ) where

import Codec.Compression.Flate qualified as FL

import Data.ByteString qualified as BS
import Data.Fallible (Fallible)
import Data.Functor ((<&>))
import Data.PDF.Filter (Filter (Filter))
import Data.PDF.FilterCombination (FilterCombination, fcBytes, mkFCAppend)
import Data.PDF.PDFObject (PDFObject (PDFName, PDFNull))
import Data.PDF.Settings (UseZopfli (UseDeflate, UseZopfli))

import PDF.Processing.FilterCombine.Rle (rle)

getCompressor :: UseZopfli -> (BS.ByteString -> Fallible BS.ByteString)
getCompressor UseZopfli  = FL.compress
getCompressor UseDeflate = FL.fastCompress

rleZopfli
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> UseZopfli
  -> Fallible FilterCombination
rleZopfli _ stream useZopfli = do
  let compressor = getCompressor useZopfli
  rle Nothing stream
    >>= compressor . fcBytes
    <&> mkFCAppend
      [ Filter (PDFName "FlateDecode")     PDFNull
      , Filter (PDFName "RunLengthDecode") PDFNull
      ]
