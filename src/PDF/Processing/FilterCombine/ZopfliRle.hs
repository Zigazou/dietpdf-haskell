module PDF.Processing.FilterCombine.ZopfliRle
  ( zopfliRle
  ) where

import Codec.Compression.Flate qualified as FL

import Data.ByteString qualified as BS
import Data.Fallible (Fallible)
import Data.Functor ((<&>))
import Data.PDF.Filter (Filter (Filter))
import Data.PDF.FilterCombination (FilterCombination, fcBytes, mkFCAppend)
import Data.PDF.PDFObject (PDFObject (PDFName, PDFNull))

import PDF.Processing.FilterCombine.Rle (rle)

zopfliRle
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> Fallible FilterCombination
zopfliRle _ stream =
  FL.compress stream
    >>= rle Nothing
    <&> mkFCAppend
      [ Filter (PDFName "RunLengthDecode") PDFNull
      , Filter (PDFName "FlateDecode")     PDFNull
      ]
      . fcBytes
