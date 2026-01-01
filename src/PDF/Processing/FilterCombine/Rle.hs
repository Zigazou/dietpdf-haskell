{-|
Run-length encoding filter combination.

Compresses a stream using RLE and returns a `FilterCombination` that appends a
`RunLengthDecode` filter with no parameters.
-}
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

{-|
Compress a stream using run-length encoding, ignoring width/components metadata.
Returns a `FilterCombination` with `RunLengthDecode` and the compressed bytes.
-}
rle
  :: Maybe (Int, Int)
  -> ByteString
  -> Fallible FilterCombination
rle _ stream =
  RL.compress stream <&> mkFCAppend [Filter (PDFName "RunLengthDecode") PDFNull]
