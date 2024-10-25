module PDF.Processing.FilterCombine.ZopfliRle
  ( zopfliRle
  ) where

import Codec.Compression.Flate qualified as FL

import Data.ByteString (ByteString)
import Data.Fallible (Fallible)
import Data.Functor ((<&>))
import Data.PDF.Filter (Filter (Filter))
import Data.PDF.FilterCombination (FilterCombination, fcBytes, mkFCAppend)
import Data.PDF.PDFObject (PDFObject (PDFName, PDFNull))
import Data.PDF.Settings (UseZopfli (UseDeflate, UseZopfli))

import PDF.Processing.FilterCombine.Rle (rle)

getCompressor :: UseZopfli -> (ByteString -> Fallible ByteString)
getCompressor UseZopfli  = FL.compress
getCompressor UseDeflate = FL.fastCompress

zopfliRle
  :: Maybe (Int, Int)
  -> ByteString
  -> UseZopfli
  -> Fallible FilterCombination
zopfliRle _ stream useZopfli = do
  let compressor = getCompressor useZopfli
  compressor stream
    >>= rle Nothing
    <&> mkFCAppend
      [ Filter (PDFName "RunLengthDecode") PDFNull
      , Filter (PDFName "FlateDecode")     PDFNull
      ]
      . fcBytes
