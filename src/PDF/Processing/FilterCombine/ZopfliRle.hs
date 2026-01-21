{-|
Zopfli/Deflate followed by RLE filter combination.

Compresses a stream with Zopfli or fast Deflate and then applies RLE, returning
a `FilterCombination` with `RunLengthDecode` followed by `FlateDecode`.
-}
module PDF.Processing.FilterCombine.ZopfliRle
  ( zopfliRle
  ) where

import Codec.Compression.Flate qualified as FL

import Data.Bitmap.BitmapConfiguration (BitmapConfiguration)
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
Apply Zopfli/Deflate first, then RLE encoding. Returns the combined
filters and compressed bytes.
-}
zopfliRle
  :: Maybe BitmapConfiguration
  -> ByteString
  -> UseZopfli
  -> Fallible FilterCombination
zopfliRle _anyBitmapConfig stream useZopfli = do
  let compressor = getCompressor useZopfli
  compressor stream
    >>= rle Nothing
    <&> mkFCAppend
      [ Filter (PDFName "RunLengthDecode") PDFNull
      , Filter (PDFName "FlateDecode")     PDFNull
      ]
      . fcBytes
