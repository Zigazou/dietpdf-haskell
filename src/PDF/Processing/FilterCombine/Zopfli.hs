{-|
Zopfli/Deflate compression filter combination.

Compresses a stream using either Zopfli or fast Deflate and returns a
`FilterCombination` that appends a `FlateDecode` filter.
-}
module PDF.Processing.FilterCombine.Zopfli
  ( zopfli
  ) where

import Codec.Compression.Flate qualified as FL

import Data.Bitmap.BitmapConfiguration (BitmapConfiguration)
import Data.ByteString (ByteString)
import Data.Fallible (Fallible)
import Data.Functor ((<&>))
import Data.PDF.Filter (Filter (Filter))
import Data.PDF.FilterCombination (FilterCombination, mkFCAppend)
import Data.PDF.PDFObject (PDFObject (PDFName, PDFNull))
import Data.PDF.Settings (UseZopfli (UseDeflate, UseZopfli))

getCompressor :: UseZopfli -> (ByteString -> Fallible ByteString)
getCompressor UseZopfli  = FL.compress
getCompressor UseDeflate = FL.fastCompress

{-|
Compress a stream with either Zopfli (slower, smaller) or fast Deflate (faster,
larger). Returns a `FilterCombination` with `FlateDecode` and the compressed
bytes.
-}
zopfli
  :: Maybe BitmapConfiguration
  -> ByteString
  -> UseZopfli
  -> Fallible FilterCombination
zopfli _anyBitmapConfig stream useZopfli = do
  let compressor = getCompressor useZopfli
  compressor stream <&> mkFCAppend [Filter (PDFName "FlateDecode") PDFNull]
