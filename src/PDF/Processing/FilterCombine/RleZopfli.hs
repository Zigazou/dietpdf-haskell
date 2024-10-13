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

import PDF.Processing.FilterCombine.Rle (rle)

rleZopfli
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> Fallible FilterCombination
rleZopfli _ stream = do
  rle Nothing stream
    >>= FL.compress . fcBytes
    <&> mkFCAppend
      [ Filter (PDFName "FlateDecode")     PDFNull
      , Filter (PDFName "RunLengthDecode") PDFNull
      ]
