{-|
This module adds bytestring formatting abilities to the Formatting module.
-}
module Formatting.ByteStringFormatter
  ( utf8
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.UTF8 qualified as BSU
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TL

import Formatting.Internal (Format (Format))

{- |
Formats a bytestring as an UTF8 string.

>>> format ("UTF8 string: " % utf8) "Fr\xC3\xA9\&d\xC3\xA9ric"
"UTF8 string: Frédéric"

-}
utf8 :: Format r (ByteString -> r)
utf8 = Format (. (TL.fromLazyText . TL.pack . BSU.toString))
