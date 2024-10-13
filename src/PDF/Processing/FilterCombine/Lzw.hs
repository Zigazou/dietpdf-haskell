module PDF.Processing.FilterCombine.Lzw
  ( lzw
  ) where

import Codec.Compression.LZW qualified as LZW

import Data.ByteString qualified as BS
import Data.Fallible (Fallible)
import Data.Functor ((<&>))
import Data.PDF.Filter (Filter (Filter))
import Data.PDF.FilterCombination (FilterCombination, mkFCAppend)
import Data.PDF.PDFObject (PDFObject (PDFName, PDFNull))

lzw
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> Fallible FilterCombination
lzw _ stream =
  LZW.compress stream <&> mkFCAppend [Filter (PDFName "LZWDecode") PDFNull]
