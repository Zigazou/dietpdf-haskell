{-|
Minimal PAM (P7) image encoder.

This module provides a tiny helper to wrap raw pixel bytes into a PAM (Portable
Arbitrary Map) container using the @P7@ format.

The produced output contains a textual PAM header followed by the unmodified
pixel payload.
-}
module Util.SimplePam (simplePam) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ColorSpace (ColorSpace, csComponents, csTupleType)

import Util.Number (fromInt)

{-|
Build a minimal PAM (@P7@) image.

Parameters:
- @width@: image width in pixels
- @height@: image height in pixels
- @colorSpace@: determines the header @DEPTH@ ('csComponents') and @TUPLTYPE@
  ('csTupleType')
- @input@: raw pixel bytes written after the @ENDHDR@ marker

This function does not validate that @input@ has the expected length.
-}
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
