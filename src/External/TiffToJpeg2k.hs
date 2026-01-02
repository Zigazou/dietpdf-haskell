{-|
Convert bitmap images to JPEG 2000 format.

Provides lossless JPEG2000 encoding via the Grok (`grk_compress`) command-line
tool, with support for CMYK and other color spaces.
-}
module External.TiffToJpeg2k (tiffToJpeg2k) where

import Data.ByteString (ByteString)
import Data.ColorSpace (ColorSpace)
import Data.Fallible (FallibleT)

import External.ExternalCommand (externalCommandBuf'')

import Util.SimpleTiff (simpleTiff)

{-|
Converts a bitmap image to a lossless JPEG 2000 image using Grok because it
supports CMYK JPEG 2000.

Encodes the input raw bitmap data as TIFF (via 'simpleTiff') then converts to
JPEG2000 using Grok `grk_compress`, which supports CMYK and other color spaces.

- @Maybe Int@: Optional quality level (0–100). 'Nothing' encodes losslessly;
'Just q' applies lossy compression at the given quality.
- @Int@: Image width in pixels
- @Int@: Image height in pixels
- @ColorSpace@: Color space of the input (determines TIFF photometric
  interpretation)
- @ByteString@: Raw bitmap data
-}
tiffToJpeg2k
  :: Maybe Int
  -> Int
  -> Int
  -> ColorSpace
  -> ByteString
  -> FallibleT IO ByteString
tiffToJpeg2k Nothing width height colorSpace input =
  externalCommandBuf'' "grk_compress"
                       [ "--in-file", "-"
                       , "--out-file", "-"
                       ]
                       "tiff"
                       "jp2"
                       (simpleTiff width height colorSpace input)
tiffToJpeg2k (Just quality) width height colorSpace input =
  externalCommandBuf'' "grk_compress"
                       [ "--in-file", "-"
                       , "--quality", show quality
                       , "--out-file", "-"
                       ]
                       "tiff"
                       "jp2"
                       (simpleTiff width height colorSpace input)
