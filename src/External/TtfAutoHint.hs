{-|
Optimize TrueType fonts using TtfAutoHint.

Provides font hinting optimization via the `ttfautohint` command-line tool.
-}
module External.TtfAutoHint (ttfAutoHintOptimize) where

import Data.ByteString (ByteString)
import Data.Fallible (FallibleT)

import External.ExternalCommand (externalCommandBuf)

{-|
Optimize a TrueType font using `ttfautohint`.

De-hints the font and removes all hinting information while ignoring font
restrictions. This reduces file size and improves cross-platform compatibility.

Returns the optimized font bytes or propagates errors from ttfautohint
execution.
-}
ttfAutoHintOptimize :: ByteString -> FallibleT IO ByteString
ttfAutoHintOptimize = externalCommandBuf "ttfautohint"
                                         [ "--dehint"
                                         , "--no-info"
                                         , "--ignore-restrictions"
                                         ]
