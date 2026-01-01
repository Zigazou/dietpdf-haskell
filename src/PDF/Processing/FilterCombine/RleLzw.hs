{-|
Run-length followed by LZW filter combination.

Applies RLE to the stream, then LZW, returning a `FilterCombination` with
`LZWDecode` and `RunLengthDecode` filters appended.
-}
module PDF.Processing.FilterCombine.RleLzw
  ( rleLzw
  ) where

import Codec.Compression.LZW qualified as LZW

import Data.ByteString (ByteString)
import Data.Fallible (Fallible)
import Data.Functor ((<&>))
import Data.PDF.Filter (Filter (Filter))
import Data.PDF.FilterCombination (FilterCombination, fcBytes, mkFCAppend)
import Data.PDF.PDFObject (PDFObject (PDFName, PDFNull))

import PDF.Processing.FilterCombine.Rle (rle)

{-|
Apply RLE first, then LZW compression. Returns the combined filters and
compressed bytes.
-}
rleLzw
  :: Maybe (Int, Int)
  -> ByteString
  -> Fallible FilterCombination
rleLzw _ stream = do
  rle Nothing stream
    >>= LZW.compress . fcBytes
    <&> mkFCAppend
      [ Filter (PDFName "LZWDecode")       PDFNull
      , Filter (PDFName "RunLengthDecode") PDFNull
      ]
