{- |
This module handles Ascii85 streams.

The ASCII base-85 encoding shall use the ASCII characters ! through u
((21h) - (75h)) and the character z (7Ah), with the 2-character sequence ~>
(7Eh)(3Eh) as its EOD marker.

All white-space characters are ignored.

Any other characters, and any character sequences that represent impossible
combinations in the ASCII base-85 encoding shall cause an error.

Specifically, ASCII base-85 encoding shall produce 5 ASCII characters for
every 4 bytes of binary data. Each group of 4 binary input bytes,
shall be converted to a group of 5 output bytes.

4 bytes of binary data shall be interpreted as a base-256 number and then shall
be converted to a base-85 number.

The five bytes of the base-85 number shall then be converted to ASCII
characters by adding 33 (the ASCII code for the character ! ) to each.

The resulting encoded data shall contain only printable ASCII characters with
codes in the range 33 (!) to 117 (u). As a special case, if all five bytes are
0, they shall be represented by the character with code 122 (z) instead of by
five exclamation points (!!!!!).

If the length of the data to be encoded is not a multiple of 4 bytes, the last,
partial group of 4 shall be used to produce a last, partial group of 5 output
characters.

Given n (1, 2, or 3) bytes of binary data, the encoder shall first append 4-n
zero bytes to make a complete group of 4. It shall encode this group in the
usual way, but shall not apply the special z case. Finally, it shall write
only the first n + 1 characters of the resulting group of 5.

These characters shall be immediately followed by the ~> EOD marker.

The following conditions shall never occur in a correctly encoded byte sequence:

- The value represented by a group of 5 characters is greater than 2^32-1.
- A z character occurs in the middle of a group.
- A final partial group contains only one character.
-}
module Codec.Filter.Ascii85
  ( decode
  , encode
  ) where

import Control.Applicative ((<|>))

import Data.Binary.Parser
    ( Get
    , anyWord8
    , endOfInput
    , label
    , many'
    , parseOnly
    , satisfy
    , skipMany
    , word8
    )
import Data.ByteString qualified as BS
import Data.Fallible (Fallible)
import Data.UnifiedError (UnifiedError (InvalidAscii85Stream))
import Data.Word (Word8)

import Pdf.Object.Object (isWhiteSpace)

import Util.Ascii
    ( asciiEXCLAMATIONMARK
    , asciiGREATERTHANSIGN
    , asciiLOWERU
    , asciiLOWERZ
    , asciiTILDE
    )

specialZeroP :: Get BS.ByteString
specialZeroP = label "specialzero" $ do
  skipMany (satisfy isWhiteSpace)
  word8 asciiLOWERZ
  return "\x00\x00\x00\x00"

isAscii85Digit :: Word8 -> Bool
isAscii85Digit byte = byte >= asciiEXCLAMATIONMARK && byte <= asciiLOWERU

a85digitP :: Get Word8
a85digitP = label "a85digit" $ do
  skipMany (satisfy isWhiteSpace)
  satisfy isAscii85Digit

baseN :: Int -> Int -> Int -> [Int]
baseN 0 _ _ = []
baseN width base value =
  let (q, r) = divMod value base in baseN (width - 1) base q ++ [r]

base256ToBase85 :: Word8 -> Word8 -> Word8 -> Word8 -> Get BS.ByteString
base256ToBase85 b1 b2 b3 b4 =
  label "base256tobase85"
    $ let b1' = fromIntegral b1 :: Int
          b2' = fromIntegral b2 :: Int
          b3' = fromIntegral b3 :: Int
          b4' = fromIntegral b4 :: Int
          c   = b1' * 16777216 + b2' * 65536 + b3' * 256 + b4'
      in  return
          . BS.pack
          . fmap ((+ asciiEXCLAMATIONMARK) . fromIntegral)
          $ baseN 5 85 c

base85ToBase256
  :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Get BS.ByteString
base85ToBase256 c1 c2 c3 c4 c5 =
  label "base85tobase256"
    $ let c1' = fromIntegral (c1 - asciiEXCLAMATIONMARK) :: Int
          c2' = fromIntegral (c2 - asciiEXCLAMATIONMARK) :: Int
          c3' = fromIntegral (c3 - asciiEXCLAMATIONMARK) :: Int
          c4' = fromIntegral (c4 - asciiEXCLAMATIONMARK) :: Int
          c5' = fromIntegral (c5 - asciiEXCLAMATIONMARK) :: Int
          b   = c1' * 52200625 + c2' * 614125 + c3' * 7225 + c4' * 85 + c5'
      in  if b >= 4294967296
            then fail "Value too big for ASCII85"
            else return . BS.pack . fmap fromIntegral $ baseN 4 256 b

fiveAscii85DigitsP :: Get BS.ByteString
fiveAscii85DigitsP = label "fiveascii85digits" $ do
  c1 <- a85digitP
  c2 <- a85digitP
  c3 <- a85digitP
  c4 <- a85digitP
  c5 <- a85digitP
  base85ToBase256 c1 c2 c3 c4 c5

fourAscii85DigitsP :: Get BS.ByteString
fourAscii85DigitsP = label "fourascii85digits" $ do
  c1 <- a85digitP
  c2 <- a85digitP
  c3 <- a85digitP
  c4 <- a85digitP
  BS.take 3 <$> base85ToBase256 c1 c2 c3 c4 0

threeAscii85DigitsP :: Get BS.ByteString
threeAscii85DigitsP = label "threeascii85digits" $ do
  c1 <- a85digitP
  c2 <- a85digitP
  c3 <- a85digitP
  BS.take 2 <$> base85ToBase256 c1 c2 c3 0 0

twoAscii85DigitsP :: Get BS.ByteString
twoAscii85DigitsP = label "twoascii85digits" $ do
  c1 <- a85digitP
  c2 <- a85digitP
  BS.take 1 <$> base85ToBase256 c1 c2 0 0 0

ascii85GroupP :: Get BS.ByteString
ascii85GroupP =
  label "ascii85group"
    $   fiveAscii85DigitsP
    <|> fourAscii85DigitsP
    <|> threeAscii85DigitsP
    <|> twoAscii85DigitsP

endOfDataMarkerP :: Get ()
endOfDataMarkerP = label "endofdatamarker" $ do
  skipMany (satisfy isWhiteSpace)
  word8 asciiTILDE
  word8 asciiGREATERTHANSIGN

decodeAscii85P :: Get BS.ByteString
decodeAscii85P = label "ascii85" $ do
  values <- many' (specialZeroP <|> ascii85GroupP)
  endOfDataMarkerP
  return (BS.concat values)

{-|
Decodes data encoded in an ASCII base-85 representation, reproducing the
original binary data.

>>> decode "87cURD_*#4DfTZ)+T~>"
Right "Hello, World!"

>>> decode "z~>"
Right "\x00\x00\x00\x00"
-}
decode
  :: BS.ByteString -- ^ Data to encode
  -> Fallible BS.ByteString
  -- ^ An `InvalidAscii85Stream` is returned if the stream is not valid
decode stream =
  let stream' = BS.filter (> 32) stream
  in case parseOnly (decodeAscii85P <* endOfInput) stream' of
      Left  msg     -> Left (InvalidAscii85Stream msg)
      Right decoded -> Right decoded

fourBytesP :: Get BS.ByteString
fourBytesP = do
  b1      <- anyWord8
  b2      <- anyWord8
  b3      <- anyWord8
  b4      <- anyWord8
  encoded <- base256ToBase85 b1 b2 b3 b4
  if encoded == "!!!!!" then return "z" else return encoded

threeBytesP :: Get BS.ByteString
threeBytesP = do
  b1 <- anyWord8
  b2 <- anyWord8
  b3 <- anyWord8
  BS.take 4 <$> base256ToBase85 b1 b2 b3 0

twoBytesP :: Get BS.ByteString
twoBytesP = do
  b1 <- anyWord8
  b2 <- anyWord8
  BS.take 3 <$> base256ToBase85 b1 b2 0 0

oneBytesP :: Get BS.ByteString
oneBytesP = do
  b1 <- anyWord8
  BS.take 2 <$> base256ToBase85 b1 0 0 0

encodeAscii85P :: Get BS.ByteString
encodeAscii85P = label "ascii85" $ do
  values <- many' (fourBytesP <|> threeBytesP <|> twoBytesP <|> oneBytesP)
  return (BS.concat values)

{-|
Encodes binary data in an ASCII base-85 representation.

>>> encode "ABCDEFGHI"
Right "5sdq,77Kd<8H~>"

>>> encode "ABCDEFGHIJ"
Right "5sdq,77Kd<8P/~>"

>>> encode "ABCDEFGHIJK"
Right "5sdq,77Kd<8P2V~z"
-}
encode
  :: BS.ByteString -- ^ Data to encode
  -> Fallible BS.ByteString
encode stream = case parseOnly (encodeAscii85P <* endOfInput) stream of
  Left  msg     -> Left (InvalidAscii85Stream msg)
  Right encoded -> Right (BS.concat [encoded, "~>"])
