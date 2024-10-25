module Util.SimplePam (simplePam) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ColorSpace (ColorSpace, csComponents, csTupleType)

import Util.Number (fromInt)

simplePam
  :: Int
  -> Int
  -> ColorSpace
  -> ByteString
  -> ByteString
simplePam width height colorSpace input =
  BS.concat
    [ "P7\n"
    , "WIDTH ", fromInt width, "\n"
    , "HEIGHT ", fromInt height, "\n"
    , "DEPTH ", fromInt (csComponents colorSpace), "\n"
    , "MAXVAL 255\n"
    , "TUPLTYPE ", csTupleType colorSpace, "\n"
    , "ENDHDR\n"
    , input
    ]
