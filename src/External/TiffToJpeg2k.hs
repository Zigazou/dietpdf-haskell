module External.TiffToJpeg2k (tiffToJpeg2k) where

import Data.ByteString (ByteString)
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
  :: Maybe Int
  -> Int
  -> Int
  -> ColorSpace
  -> ByteString
  -> FallibleT IO ByteString
tiffToJpeg2k Nothing width height colorSpace input =
  externalCommandBuf'' "grk_compress"
                       [ "-in_file", "-"
                       , "-out_file", "-"
                       ]
                       "tiff"
                       "jp2"
                       (simpleTiff width height colorSpace input)
tiffToJpeg2k (Just quality) width height colorSpace input =
  externalCommandBuf'' "grk_compress"
                       [ "-in_file", "-"
                       , "-quality", show quality
                       , "-out_file", "-"
                       ]
                       "tiff"
                       "jp2"
                       (simpleTiff width height colorSpace input)
