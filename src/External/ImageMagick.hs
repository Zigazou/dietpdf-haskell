{-|
Optimize JPEG files using JpegTran.

Provides JPEG optimization via the `jpegtran` command-line tool, comparing and
selecting between progressive and baseline encoding modes.
-}
module External.ImageMagick (extractCbCrChannels) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Fallible (FallibleT)

import External.ExternalCommand (externalCommandBuf)

{-|
Extract the Cb and Cr channels from a JPEG image in the YCbCr color space.

This is a wrapper around ImageMagick's `convert` command-line tool, used to
inspect pixel values (e.g. with `containsOnlyGray`) before deciding whether a
JPEG can be safely reduced to grayscale.
-}
extractCbCrChannels :: ByteString -> FallibleT IO (ByteString, ByteString)
extractCbCrChannels input = do
  output <- externalCommandBuf "convert"
                               ["jpg:-", "-colorspace", "YCbCr", "yuv:-"]
                               input

  let
    componentLength :: Int
    componentLength = BS.length output `div` 6

    cb_start :: Int
    cb_start = 4 * componentLength

    cr_start :: Int
    cr_start = 5 * componentLength

    cb_component :: ByteString
    cb_component = BS.take componentLength (BS.drop cb_start output)

    cr_component :: ByteString
    cr_component = BS.take componentLength (BS.drop cr_start output)

  return (cb_component, cr_component)
