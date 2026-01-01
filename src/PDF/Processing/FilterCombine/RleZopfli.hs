{-|
Run-length followed by Zopfli/Deflate filter combination.

Applies RLE to the stream and then compresses with either Zopfli or fast
Deflate, returning a `FilterCombination` with `FlateDecode` and
`RunLengthDecode` filters appended.
-}
module PDF.Processing.FilterCombine.RleZopfli
  ( rleZopfli
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

{-|
Apply RLE first, then Zopfli/Deflate compression. Returns the combined filters
and compressed bytes.
-}
rleZopfli
  :: Maybe (Int, Int)
  -> ByteString
  -> UseZopfli
  -> Fallible FilterCombination
rleZopfli _ stream useZopfli = do
  let compressor = getCompressor useZopfli
  rle Nothing stream
    >>= compressor . fcBytes
    <&> mkFCAppend
      [ Filter (PDFName "FlateDecode")     PDFNull
      , Filter (PDFName "RunLengthDecode") PDFNull
      ]
