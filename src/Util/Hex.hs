{-# LANGUAGE OverloadedStrings   #-}

{- |
This modules contains functions to help encoding/decoding hexadecimal strings.
-}
module Util.Hex
  ( fromHexDigits
  , toHexDigits
  ) where

import qualified Data.ByteString               as BS
import           Data.List.Split                ( chunksOf )
import           Data.Word                      ( Word8 )
import           Util.Ascii                     ( asciiDIGITNINE
                                                , asciiDIGITZERO
                                                , asciiLOWERA
                                                , asciiLOWERF
                                                , asciiSPACE
                                                , asciiUPPERA
                                                , asciiUPPERF
                                                )

(><) :: Word8 -> (Word8, Word8) -> Bool
(><) byte (lower, upper) = byte >= lower && byte <= upper

fromHexDigit :: Word8 -> Maybe Word8
fromHexDigit byte
  | byte >< (asciiDIGITZERO, asciiDIGITNINE) = Just $ byte - asciiDIGITZERO
  | byte >< (asciiLOWERA, asciiLOWERF) = Just $ 10 + byte - asciiLOWERA
  | byte >< (asciiUPPERA, asciiUPPERF) = Just $ 10 + byte - asciiUPPERA
  | otherwise                          = Nothing

{- |
Convert an hexadecimal string to a string of bytes.

>>> fromHexDigits "AB"
"\xAB"

>>> fromHexDigits "  A   B "
"\xAB"

>>> fromHexDigits "abcdefgh12"
"\xAB\xCD\xEF\x12"
-}
fromHexDigits :: BS.ByteString -> BS.ByteString
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

toHexDigit :: Word8 -> Word8
toHexDigit byte | byte < 10 = byte + asciiDIGITZERO
                | byte < 16 = byte + asciiLOWERA - 10
                | otherwise = asciiSPACE

toHex :: Word8 -> BS.ByteString
toHex byte = BS.cons (toHexDigit upper) (BS.singleton . toHexDigit $ lower)
  where (upper, lower) = divMod byte 16

{- |
Convert a string of bytes to an hexadecimal string.

>>> toHexDigits "\x12\x34\x56\x78\x9a\xbc\xde\xf0"
"123456789abcdef0"
-}
toHexDigits :: BS.ByteString -> BS.ByteString
toHexDigits = BS.concat . BS.foldr ((:) . toHex) []
