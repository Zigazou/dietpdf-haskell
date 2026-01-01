module External.JpegToJpeg2k (jpegToJpeg2k) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Search (indices)
import Data.Fallible (FallibleT)

import External.ExternalCommand (externalCommandBuf'')

getJpegComponents :: ByteString -> Int
getJpegComponents jpegImage =
  let ffc0 = indices "\xff\xc0" jpegImage
      ffc2 = indices "\xff\xc2" jpegImage
  in case (ffc0, ffc2) of
       (offset:_, _) -> fromIntegral (BS.index jpegImage (offset + 9))
       (_, offset:_) -> fromIntegral (BS.index jpegImage (offset + 9))
       _default      -> 3

{-|
Converts a JPEG image to a lossy JPEG 2000 image.

This function is a wrapper around the `grk_compress` command-line tool.

The input JPEG image can be in any color space (RGB, CMYK, Grayscale). The
output JPEG 2000 image will be in the same color space as the input JPEG image.
The quality of the output JPEG 2000 image is determined by the `quality`
parameter.

If the input JPEG image is in CMYK color space, this function uses ImageMagick
to convert the JPEG image to a TIFF image because it can keep CMYK colorspace
in the process. It is also necessary to negate the image. Then, it uses Grok to
convert the TIFF image to a JPEG 2000 image.
-}
jpegToJpeg2k :: Int -> ByteString -> FallibleT IO ByteString
jpegToJpeg2k quality jpegImage =
  case getJpegComponents jpegImage of
    4 -> do
      -- Use ImageMagick to convert the JPEG image to a TIFF image because it
      -- can keep CMYK colorspace in the process. It is also necessary to
      -- negate the image.
      tiffImage <- externalCommandBuf'' "convert"
                                        ["-" , "-negate", "-"]
                                        "jpg"
                                        "tiff"
                                        jpegImage

      externalCommandBuf'' "grk_compress"
                          [ "--in-file", "-"
                          , "--quality", show quality
                          , "--out-file", "-"
                          ]
                          "tiff"
                          "jp2"
                          tiffImage
    _anyOtherCase -> do
      -- For other types of JPEG (RGB, Grayscale), Grok is able to read them.
      -- It also avoids using ImageMagick which can change the colorspace due
      -- to a bug in color space handling before ImageMagick v6.9.13.
      externalCommandBuf'' "grk_compress"
                          [ "--in-file", "-"
                          , "--quality", show quality
                          , "--out-file", "-"
                          ]
                          "jpg"
                          "jp2"
                          jpegImage
