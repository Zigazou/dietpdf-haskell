module PDF.Processing.FilterCombine.Zopfli
  ( zopfli
  ) where

import Codec.Compression.Flate qualified as FL

import Data.ByteString (ByteString)
import Data.Fallible (Fallible)
import Data.Functor ((<&>))
import Data.PDF.Filter (Filter (Filter))
import Data.PDF.FilterCombination (FilterCombination, mkFCAppend)
import Data.PDF.PDFObject (PDFObject (PDFName, PDFNull))
import Data.PDF.Settings (UseZopfli (UseDeflate, UseZopfli))

getCompressor :: UseZopfli -> (ByteString -> Fallible ByteString)
getCompressor UseZopfli  = FL.compress
getCompressor UseDeflate = FL.fastCompress

zopfli
  :: Maybe (Int, Int)
  -> ByteString
  -> UseZopfli
  -> Fallible FilterCombination
zopfli _ stream useZopfli = do
  let compressor = getCompressor useZopfli
  compressor stream <&> mkFCAppend [Filter (PDFName "FlateDecode") PDFNull]
