{-|
This module adds bytestring formatting abilities to the Formatting module.
-}
module Formatting.ByteStringFormatter
  ( utf8
  ) where

import qualified Data.ByteString               as BS
import qualified Data.ByteString.UTF8          as BSU
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.Builder        as TL
import           Formatting.Internal            ( Format(Format) )

{- |
Formats a bytestring as an UTF8 string.

>>> format ("UTF8 string: " % utf8) "Fr\xC3\xA9\&d\xC3\xA9ric"
"UTF8 string: Frédéric"

-}
utf8 :: Format r (BS.ByteString -> r)
utf8 = Format (. (TL.fromLazyText . TL.pack . BSU.toString))
