module External.TtfAutoHint (ttfAutoHintOptimize) where

import Data.ByteString qualified as BS
import Data.Fallible (FallibleT)

import External.ExternalCommand (externalCommandBuf)

ttfAutoHintOptimize :: BS.ByteString -> FallibleT IO BS.ByteString
ttfAutoHintOptimize = externalCommandBuf "ttfautohint"
                                         [ "--dehint"
                                         , "--no-info"
                                         , "--ignore-restrictions"
                                         ]
