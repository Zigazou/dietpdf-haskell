module External.PamToJpeg2k (pamToJpeg2k) where

import Data.ByteString qualified as BS

import External.ExternalCommand (externalCommandBuf)

import Util.Number (fromInt)
import Util.UnifiedError (FallibleT)

pamToJpeg2k
  :: Int
  -> Int
  -> Int
  -> BS.ByteString
  -> BS.ByteString
  -> FallibleT IO BS.ByteString
pamToJpeg2k width height depth tupltype input =
  let pamImage = BS.concat
        [ "P7\n"
        , "WIDTH ", fromInt width, "\n"
        , "HEIGHT ", fromInt height, "\n"
        , "DEPTH ", fromInt depth, "\n"
        , "MAXVAL 255\n"
        , "TUPLTYPE ", tupltype, "\n"
        , "ENDHDR\n"
        , input
        ]
  in externalCommandBuf "pamtojpeg2k" [] pamImage
