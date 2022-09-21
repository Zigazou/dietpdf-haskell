{- |
This module holds function handling hexadecimal strings.
-}
module Codec.Filter.AsciiHex
  ( encode
  , decode
  ) where

import qualified Data.ByteString               as BS
import           Util.Ascii                     ( asciiGREATERTHANSIGN )
import           Util.Errors                    ( UnifiedError )
import           Util.Hex                       ( fromHexDigits
                                                , toHexDigits
                                                )

{- |
Decodes an hexadecimal bytestring.

Any character not included in `0123456789abcdefABCDEF` is simply ignored.

If there is an odd number of hexadecimal digits, a `0` is appended to make it
even.
-}
decode :: BS.ByteString -> Either UnifiedError BS.ByteString
decode = return . fromHexDigits

{- |
Encodes an hexadecimal bytestring.

A `>` is appended to the output string to comply with the PDF specification.
-}
encode :: BS.ByteString -> Either UnifiedError BS.ByteString
encode = return . flip BS.snoc asciiGREATERTHANSIGN . toHexDigits
