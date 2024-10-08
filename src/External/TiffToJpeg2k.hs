module External.TiffToJpeg2k (tiffToJpeg2k) where

import Data.ByteString qualified as BS
import Data.ColorSpace (ColorSpace)
import Data.Fallible (FallibleT)

import External.ExternalCommand (externalCommandBuf'')

import Util.SimpleTiff (simpleTiff)

{- |
Converts a bitmap image to a lossless JPEG 2000 image using Grok because it
supports CMYK JPEG 2000.

This function is a wrapper around the `grk_compress` command-line tool.
-}
tiffToJpeg2k
  :: Int
  -> Int
  -> ColorSpace
  -> BS.ByteString
  -> FallibleT IO BS.ByteString
tiffToJpeg2k width height colorSpace input =
  externalCommandBuf'' "grk_compress"
                       [ "--in-file", "-"
                       , "--out-file", "-"
                       ]
                       "tiff"
                       "jp2"
                       (simpleTiff width height colorSpace input)
