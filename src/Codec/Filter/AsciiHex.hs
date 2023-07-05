{- |
This module holds function handling hexadecimal strings.

It decodes data that has been encoded in ASCII hexadecimal form.

ASCII hexadecimal encoding converts binary data, such as image data or
previously compressed data, to 7-bit ASCII characters.

It expands the data by a factor 1:2.

Decoding shall produce one byte of binary data for each pair of ASCII
hexadecimal digits (0–9 and A–F or a–f).

All white-space characters are ignored.

A GREATER-THAN SIGN (3Eh) indicates EOD.

Any other characters shall cause an error.

If the filter encounters the EOD marker after reading an odd number of
hexadecimal digits, it shall behave as if a 0 (zero) followed the last digit.
-}
module Codec.Filter.AsciiHex
  ( encode
  , decode
  ) where

import qualified Data.ByteString               as BS
import           Util.Ascii                     ( asciiGREATERTHANSIGN )
import           Util.UnifiedError                    ( UnifiedError )
import           Util.Hex                       ( fromHexDigits
                                                , toHexDigits
                                                )

{- |
Decodes an hexadecimal bytestring.

Any character not included in `0123456789abcdefABCDEF` is simply ignored.

If there is an odd number of hexadecimal digits, a `0` is appended to make it
even.

>>> decode "AB"         
Right "\xAB"

>>> decode "1"          
Right "\x10"

>>> decode "abcdefgh12" 
Right "\xAB\xCD\xEF\x12"

>>> decode "abcdefgh1yz"
Right "\xAB\xCD\xEF\x10"
-}
decode :: BS.ByteString -> Either UnifiedError BS.ByteString
decode = return . fromHexDigits

{- |
Encodes an hexadecimal bytestring.

A `>` is appended to the output string to comply with the PDF specification.

>>> encode "\x12\x34\x56\x78\x9a\xbc\xde\xf0"
Right "123456789abcdef0"
-}
encode :: BS.ByteString -> Either UnifiedError BS.ByteString
encode = return . flip BS.snoc asciiGREATERTHANSIGN . toHexDigits
