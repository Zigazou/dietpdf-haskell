module PDF.Processing.FilterCombine.Rle
  ( rle
  ) where

import Codec.Compression.RunLength qualified as RL

import Data.ByteString (ByteString)
import Data.Fallible (Fallible)
import Data.Functor ((<&>))
import Data.PDF.Filter (Filter (Filter))
import Data.PDF.FilterCombination (FilterCombination, mkFCAppend)
import Data.PDF.PDFObject (PDFObject (PDFName, PDFNull))

rle
  :: Maybe (Int, Int)
  -> ByteString
  -> Fallible FilterCombination
rle _ stream =
  RL.compress stream <&> mkFCAppend [Filter (PDFName "RunLengthDecode") PDFNull]
