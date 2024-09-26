module External.PamToJpeg2k (pamToJpeg2k, jpegToJpeg2k) where

import Data.ByteString qualified as BS
import Data.UnifiedError (FallibleT)

import External.ExternalCommand (externalCommandBuf, externalCommandBuf')

import Util.Number (fromInt)

createPamImage
  :: Int
  -> Int
  -> Int
  -> BS.ByteString
  -> BS.ByteString
  -> BS.ByteString
createPamImage width height depth tupltype input =
  BS.concat
    [ "P7\n"
    , "WIDTH ", fromInt width, "\n"
    , "HEIGHT ", fromInt height, "\n"
    , "DEPTH ", fromInt depth, "\n"
    , "MAXVAL 255\n"
    , "TUPLTYPE ", tupltype, "\n"
    , "ENDHDR\n"
    , input
    ]

{- |
Converts a bitmap image to a lossless JPEG 2000 image.
-}
pamToJpeg2k
  :: Int
  -> Int
  -> Int
  -> BS.ByteString
  -> BS.ByteString
  -> FallibleT IO BS.ByteString
pamToJpeg2k width height depth tupltype input =
  let pamImage = createPamImage width height depth tupltype input
  in externalCommandBuf "pamtojpeg2k" [] pamImage

{- |
Converts a JPEG image to a lossy JPEG 2000 image.
-}
jpegToJpeg2k
  :: Int
  -> BS.ByteString
  -> FallibleT IO BS.ByteString
jpegToJpeg2k quality input = do
  pnmImage <- externalCommandBuf' "jpegtopnm"
                                  [ "-quiet", "-dct", "float"]
                                  input

  externalCommandBuf "pamtojpeg2k" [ "-mode=real"
                                   , "-compression=" ++ show quality
                                   ]
                                   pnmImage
