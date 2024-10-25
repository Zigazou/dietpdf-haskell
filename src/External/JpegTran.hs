-- | Optimize a JPG file using JpegTran.
module External.JpegTran (jpegtranOptimize) where

import Codec.Compression.Flate (fastCompress)

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Fallible (FallibleT)

import External.ExternalCommand (externalCommandBuf)

{- |
Optimize a JPG file using `jpegtran`.

This function is a wrapper around the `jpegtran` command-line tool. It
optimizes the input JPG file by removing all metadata and optimizing the
compression.

To find the better optimization, this function runs `jpegtran` twice: once
with the `-progressive` flag and once without it. It then compares the
compressed sizes of the two outputs and returns the smaller one.
-}
jpegtranOptimize :: ByteString -> FallibleT IO ByteString
jpegtranOptimize input = do
  progressive <- externalCommandBuf
                  "jpegtran"
                  ["-optimize", "-progressive", "-copy", "none"]
                  input

  baseline <- externalCommandBuf
                  "jpegtran"
                  ["-optimize", "-copy", "none"]
                  input

  let baselineLength = case fastCompress baseline of
                        Right compressed -> BS.length compressed
                        _anythingElse    -> BS.length baseline

  return $ if baselineLength < BS.length progressive then baseline
                                                     else progressive
