module External.TtfAutoHint (ttfAutoHintOptimize) where

import Data.ByteString (ByteString)
import Data.Fallible (FallibleT)

import External.ExternalCommand (externalCommandBuf)

{- |
Optimize a TTF font using `ttfautohint`.

This function is a wrapper around the `ttfautohint` command-line tool. It
de-hints the font, removes hinting information, and ignores restrictions.
-}
ttfAutoHintOptimize :: ByteString -> FallibleT IO ByteString
ttfAutoHintOptimize = externalCommandBuf "ttfautohint"
                                         [ "--dehint"
                                         , "--no-info"
                                         , "--ignore-restrictions"
                                         ]
