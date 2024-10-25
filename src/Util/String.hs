-- | This modules contains functions to help encoding of PDF strings.
module Util.String
  ( fromString
  , fromHexString
  , normalizeHexString
  , hexStringToString
  , startsWith
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word (Word8)

import Util.Ascii
    ( asciiDIGITNINE
    , asciiDIGITZERO
    , asciiLOWERA
    , asciiLOWERF
    , asciiUPPERA
    , asciiUPPERF
    , pattern AsciiCR
    , pattern AsciiLEFTPARENTHESIS
    , pattern AsciiREVERSESOLIDUS
    , pattern AsciiRIGHTPARENTHESIS
    )

escapeChar :: Word8 -> ByteString
escapeChar AsciiCR               = "\\r"
escapeChar AsciiLEFTPARENTHESIS  = "\\("
escapeChar AsciiRIGHTPARENTHESIS = "\\)"
escapeChar AsciiREVERSESOLIDUS   = "\\\\"
escapeChar byte                  = BS.singleton byte

{-|
Encode a PDF string.

It escapes `asciiCR` and parenthesis characters.

It does not try optimize the string by avoiding unneeded escaping of
well-balanced parenthesis.
-}
fromString :: ByteString -> ByteString
fromString string = BS.concat ["(", BS.concatMap escapeChar string, ")"]

onlyHexChar :: Word8 -> ByteString
onlyHexChar byte
  | byte >= asciiDIGITZERO && byte <= asciiDIGITNINE = BS.singleton byte
  | byte >= asciiLOWERA && byte <= asciiLOWERF = BS.singleton byte
  | byte >= asciiUPPERA && byte <= asciiUPPERF = BS.singleton (lowerDigit byte)
  | otherwise = ""
  where lowerDigit d = d - asciiUPPERA + asciiLOWERA

{- |
Normalize an hexadecimal string.

It:

- removes whitespaces
- handles zero at the end of the hexadecimal string
- converts every character to lowercase
-}
normalizeHexString :: ByteString -> ByteString
normalizeHexString ""        = ""
normalizeHexString hexString = if stringLengthIsEven && lastDigitIsZero
  then normalizedWithoutLastDigit
  else normalized
 where
  normalized                 = BS.concatMap onlyHexChar hexString
  normalizedWithoutLastDigit = BS.take (BS.length normalized - 1) normalized
  lastDigitIsZero =
       (normalized /= "")
    && (asciiDIGITZERO == BS.index normalized (BS.length normalized - 1))
  stringLengthIsEven = even (BS.length normalized)

{-|
Encode a PDF hexstring.

Non hexadecimal digits are completely discarded.

A trailing zero is removed if the number of hexadecimal digit is even.
-}
fromHexString :: ByteString -> ByteString
fromHexString hexString = BS.concat ["<", normalizeHexString hexString, ">"]

digitToNumber :: Word8 -> Word8
digitToNumber byte
  | byte >= asciiDIGITZERO && byte <= asciiDIGITNINE = byte - asciiDIGITZERO
  | byte >= asciiLOWERA && byte <= asciiLOWERF = 10 + byte - asciiLOWERA
  | byte >= asciiUPPERA && byte <= asciiUPPERF = 10 + byte - asciiUPPERA
  | otherwise = 0

{-|
Converts an hexadecimal string to a string.

Non hexadecimal digits are completely discarded.

If the number of hexadecimal digit is odd, a trailing zero is appended.
-}
hexStringToString :: ByteString -> ByteString
hexStringToString hexString = BS.concat
  [convert . normalizeHexString $ hexString]
 where
  convertHexByte h l = digitToNumber h * 16 + digitToNumber l
  convert hs
    | BS.length hs == 0 = ""
    | BS.length hs == 1 = BS.singleton (convertHexByte (BS.index hs 0) 0)
    | otherwise = BS.cons (convertHexByte (BS.index hs 0) (BS.index hs 1))
                          (convert (BS.drop 2 hs))

{- |
Tells if a `ByteString` starts with another `ByteString`.
-}
startsWith :: ByteString -> ByteString -> Bool
startsWith start str = BS.take (BS.length start) str == start
