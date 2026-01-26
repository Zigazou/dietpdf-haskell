{-|
Zopfli/Deflate compression filter combination.

Compresses a stream using either Zopfli or fast Deflate and returns a
`FilterCombination` that appends a `FlateDecode` filter.
-}
module PDF.Processing.FilterCombine.Compressor
  ( compressor
  ) where

import Codec.Compression.BrotliForPDF qualified as BR
import Codec.Compression.Flate qualified as FL

import Data.Bitmap.BitmapConfiguration (BitmapConfiguration)
import Data.ByteString (ByteString)
import Data.Fallible (Fallible)
import Data.Functor ((<&>))
import Data.PDF.Filter (Filter (Filter))
import Data.PDF.FilterCombination (FilterCombination, mkFCAppend)
import Data.PDF.PDFObject (PDFObject (PDFName, PDFNull))
import Data.PDF.Settings (UseCompressor (UseBrotli, UseDeflate, UseZopfli))

getCompressor
  :: UseCompressor
  -> (ByteString -> Fallible ByteString, PDFObject)
getCompressor UseZopfli  = (FL.compress    , PDFName "FlateDecode" )
getCompressor UseDeflate = (FL.fastCompress, PDFName "FlateDecode" )
getCompressor UseBrotli  = (BR.compress    , PDFName "BrotliDecode")

{-|
Compress a stream with either Zopfli (slower, smaller) or fast Deflate (faster,
larger). Returns a `FilterCombination` with `FlateDecode` and the compressed
bytes.
-}
compressor
  :: Maybe BitmapConfiguration
  -> ByteString
  -> UseCompressor
  -> Fallible FilterCombination
compressor _anyBitmapConfig stream useCompressor = do
  let (compress, filterName) = getCompressor useCompressor
  compress stream <&> mkFCAppend [Filter filterName PDFNull]