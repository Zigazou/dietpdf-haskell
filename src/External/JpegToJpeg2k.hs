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
  -- Use ImageMagick to convert the JPEG image to a TIFF image because it can
  -- keep CMYK colorspace in the process.
  let components = getJpegComponents jpegImage
      params = if components == 4 then ["-" , "-negate", "-"]
                                  else ["-", "-"  ]

  tiffImage <- externalCommandBuf'' "convert" params "jpg" "tiff" jpegImage

  externalCommandBuf'' "grk_compress"
                       [ "--in-file", "-"
                       , "--quality", show quality
                       , "--out-file", "-"
                       , "--verbose"
                       ]
                       "tiff"
                       "j2k"
                       tiffImage
