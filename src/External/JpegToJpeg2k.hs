module External.JpegToJpeg2k (jpegToJpeg2k) where

import Data.ByteString qualified as BS
import Data.ByteString.Search (indices)
import Data.Fallible (FallibleT)

import External.ExternalCommand (externalCommandBuf'')

getJpegComponents :: BS.ByteString -> Int
getJpegComponents jpegImage =
  let ffc0 = indices "\xff\xc0" jpegImage
      ffc2 = indices "\xff\xc2" jpegImage
  in case (ffc0, ffc2) of
       (offset:_, _) -> fromIntegral (BS.index jpegImage (offset + 9))
       (_, offset:_) -> fromIntegral (BS.index jpegImage (offset + 9))
       _             -> 3

{- |
Converts a JPEG image to a lossy JPEG 2000 image.
-}
jpegToJpeg2k :: Int -> BS.ByteString -> FallibleT IO BS.ByteString
jpegToJpeg2k quality jpegImage = do
  if getJpegComponents jpegImage == 4
    then do
      -- Use ImageMagick to convert the JPEG image to a TIFF image because it
      -- can keep CMYK colorspace in the process.
      tiffImage <- externalCommandBuf'' "convert"
                                        ["-" , "-negate", "-"]
                                        "jpg"
                                        "tiff"
                                        jpegImage

      externalCommandBuf'' "grk_compress"
                          [ "--in-file", "-"
                          , "--quality", show quality
                          , "--out-file", "-"
                          , "--verbose"
                          ]
                          "tiff"
                          "j2k"
                          tiffImage
    else
      -- For other types of JPEG (RGB, Grayscale), Grok is able to read them.
      -- It also avoids using ImageMagick which can change the colorspace due
      -- to a bug in color space handling before ImageMagick v6.9.13.
      externalCommandBuf'' "grk_compress"
                          [ "--in-file", "-"
                          , "--quality", show quality
                          , "--out-file", "-"
                          , "--verbose"
                          ]
                          "jpg"
                          "j2k"
                          jpegImage
