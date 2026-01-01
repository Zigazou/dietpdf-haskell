{-|
Deflate compression filter combination.

Provides a helper to compress a stream using fast Deflate and return a
`FilterCombination` that appends a `FlateDecode` filter with no parameters.
-}
module PDF.Processing.FilterCombine.Deflate
  ( deflate
  ) where

import Codec.Compression.Flate qualified as FL

import Data.ByteString (ByteString)
import Data.Fallible (Fallible)
import Data.Functor ((<&>))
import Data.PDF.Filter (Filter (Filter))
import Data.PDF.FilterCombination (FilterCombination, mkFCAppend)
import Data.PDF.PDFObject (PDFObject (PDFName, PDFNull))

{-|
Compress a stream using fast Deflate, ignoring width/components metadata.

Returns a `FilterCombination` with a `FlateDecode` filter and the compressed
bytes.
-}
deflate
  :: Maybe (Int, Int)
  -> ByteString
  -> Fallible FilterCombination
deflate _ stream =
  FL.fastCompress stream <&> mkFCAppend [Filter (PDFName "FlateDecode") PDFNull]
