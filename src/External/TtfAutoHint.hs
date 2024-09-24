module External.TtfAutoHint (ttfAutoHintOptimize) where

import Data.ByteString qualified as BS

import External.ExternalCommand (externalCommandBuf)

import Util.UnifiedError (FallibleT)

ttfAutoHintOptimize :: BS.ByteString -> FallibleT IO BS.ByteString
ttfAutoHintOptimize = externalCommandBuf "ttfautohint" 
                                         [ "--dehint"
                                         , "--no-info"
                                         , "--ignore-restrictions"
                                         ]
