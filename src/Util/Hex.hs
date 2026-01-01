{-|
Hexadecimal encoding and decoding for 'ByteString'.

This module provides small utilities to convert between raw bytes ('ByteString')
and their hexadecimal textual representation.

The decoding function is forgiving: it ignores any non-hex characters (including
whitespace) and will treat an odd number of hex digits as if the missing low
nibble were zero.
-}
module Util.Hex
  ( fromHexDigits
  , toHexDigits
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.List.Split (chunksOf)
import Data.Word (Word8)

import Util.Ascii
  ( asciiDIGITNINE
  , asciiDIGITZERO
  , asciiLOWERA
  , asciiLOWERF
  , asciiSPACE
  , asciiUPPERA
  , asciiUPPERF
  )

{-|
Inclusive range check for a byte.

This is a small helper used to test whether an input byte belongs to a
particular ASCII interval.
-}
infixl 9 ><
(><) :: Word8 -> (Word8, Word8) -> Bool
(><) byte (lower, upper) = byte >= lower && byte <= upper

{-|
Decode a single ASCII hex digit into its numeric value (0..15).

Accepts both lowercase (@a..f@) and uppercase (@A..F@). Returns 'Nothing' for
non-hex bytes.
-}
fromHexDigit :: Word8 -> Maybe Word8
fromHexDigit byte
  | byte >< (asciiDIGITZERO, asciiDIGITNINE) = Just $ byte - asciiDIGITZERO
  | byte >< (asciiLOWERA, asciiLOWERF) = Just $ 10 + byte - asciiLOWERA
  | byte >< (asciiUPPERA, asciiUPPERF) = Just $ 10 + byte - asciiUPPERA
  | otherwise                          = Nothing

{-|
Convert an hexadecimal string to a string of bytes.

>>> fromHexDigits "AB"
"\xAB"

>>> fromHexDigits "  A   B "
"\xAB"

>>> fromHexDigits "abcdefgh12"
"\xAB\xCD\xEF\x12"
-}
fromHexDigits :: ByteString -> ByteString
fromHexDigits = BS.pack . fmap toHexByte . chunksOf 2 . BS.foldr toHexBytes []
 where
  toHexBytes :: Word8 -> [Word8] -> [Word8]
  toHexBytes digit digits = case fromHexDigit digit of
    Just byte -> byte : digits
    Nothing   -> digits

  toHexByte :: [Word8] -> Word8
  toHexByte [upper, lower] = upper * 16 + lower
  toHexByte [upper]        = upper * 16
  toHexByte _              = 0

{-|
Encode a 4-bit value (0..15) as a lowercase ASCII hex digit.

Values outside @0..15@ are encoded as a space character.
-}
toHexDigit :: Word8 -> Word8
toHexDigit byte | byte < 10 = byte + asciiDIGITZERO
                | byte < 16 = byte + asciiLOWERA - 10
                | otherwise = asciiSPACE

{-|
Encode a single byte as two lowercase hexadecimal digits.
-}
toHex :: Word8 -> ByteString
toHex byte = BS.cons (toHexDigit upper) (BS.singleton . toHexDigit $ lower)
  where (upper, lower) = divMod byte 16

{-|
Convert a string of bytes to an hexadecimal string.

>>> toHexDigits "\x12\x34\x56\x78\x9a\xbc\xde\xf0"
"123456789abcdef0"
-}
toHexDigits :: ByteString -> ByteString
toHexDigits = BS.concat . BS.foldr ((:) . toHex) []
