{-|
Run-length followed by Zopfli/Deflate filter combination.

Applies RLE to the stream and then compresses with either Zopfli or fast
Deflate, returning a `FilterCombination` with `FlateDecode` and
`RunLengthDecode` filters appended.
-}
module PDF.Processing.FilterCombine.RleCompressor
  ( rleCompressor
  ) where

import Codec.Compression.BrotliForPDF qualified as BR
import Codec.Compression.Flate qualified as FL

import Data.Bitmap.BitmapConfiguration (BitmapConfiguration)
import Data.ByteString (ByteString)
import Data.Fallible (Fallible)
import Data.Functor ((<&>))
import Data.PDF.Filter (Filter (Filter))
import Data.PDF.FilterCombination (FilterCombination, fcBytes, mkFCAppend)
import Data.PDF.PDFObject (PDFObject (PDFName, PDFNull))
import Data.PDF.Settings (UseCompressor (UseBrotli, UseDeflate, UseZopfli))

import PDF.Processing.FilterCombine.Rle (rle)

getCompressor :: UseCompressor -> (ByteString -> Fallible ByteString, PDFObject)
getCompressor UseZopfli  = (FL.compress    , PDFName "FlateDecode" )
getCompressor UseDeflate = (FL.fastCompress, PDFName "FlateDecode" )
getCompressor UseBrotli  = (BR.compress    , PDFName "BrotliDecode")

{-|
Apply RLE first, then Zopfli/Deflate compression. Returns the combined filters
and compressed bytes.
-}
rleCompressor
  :: Maybe BitmapConfiguration
  -> ByteString
  -> UseCompressor
  -> Fallible FilterCombination
rleCompressor _anyBitmapConfig stream useCompressor = do
  let (compressor, filterName) = getCompressor useCompressor
  rle Nothing stream
    >>= compressor . fcBytes
    <&> mkFCAppend
      [ Filter filterName                  PDFNull
      , Filter (PDFName "RunLengthDecode") PDFNull
      ]
