module PDF.Processing.FilterCombine.Zopfli
  ( zopfli
  ) where

import Codec.Compression.Flate qualified as FL

import Data.ByteString qualified as BS
import Data.Fallible (Fallible)
import Data.Functor ((<&>))
import Data.PDF.Filter (Filter (Filter))
import Data.PDF.FilterCombination (FilterCombination, mkFCAppend)
import Data.PDF.PDFObject (PDFObject (PDFName, PDFNull))

zopfli
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> Fallible FilterCombination
zopfli _ stream =
  FL.compress stream <&> mkFCAppend [Filter (PDFName "FlateDecode") PDFNull]
