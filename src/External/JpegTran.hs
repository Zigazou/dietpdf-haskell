-- | Optimize a JPG file using JpegTran.
module External.JpegTran (jpegtranOptimize) where

import Data.ByteString qualified as BS

import External.ExternalCommand (externalCommandBuf)

import Util.UnifiedError (FallibleT)

import Codec.Compression.Flate (fastCompress)

jpegtranOptimize :: BS.ByteString -> FallibleT IO BS.ByteString
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