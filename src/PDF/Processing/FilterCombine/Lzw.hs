{-|
LZW compression filter combination.

Compresses a stream using LZW and returns a `FilterCombination` that appends an
`LZWDecode` filter with no parameters.
-}
module PDF.Processing.FilterCombine.Lzw
  ( lzw
  ) where

import Codec.Compression.LZW qualified as LZW

import Data.ByteString (ByteString)
import Data.Fallible (Fallible)
import Data.Functor ((<&>))
import Data.PDF.Filter (Filter (Filter))
import Data.PDF.FilterCombination (FilterCombination, mkFCAppend)
import Data.PDF.PDFObject (PDFObject (PDFName, PDFNull))

{-|
Compress a stream using LZW, ignoring width/components metadata.

Returns a `FilterCombination` with `LZWDecode` and the compressed bytes.
-}
lzw
  :: Maybe (Int, Int)
  -> ByteString
  -> Fallible FilterCombination
lzw _ stream =
  LZW.compress stream <&> mkFCAppend [Filter (PDFName "LZWDecode") PDFNull]
