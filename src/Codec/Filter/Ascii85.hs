{-# LANGUAGE OverloadedStrings #-}
-- | 
module Codec.Filter.Ascii85
  ( decode
  , encode
  ) where

import           Control.Applicative            ( (<|>) )
import           Data.Binary.Parser             ( Get
                                                , anyWord8
                                                , endOfInput
                                                , label
                                                , many'
                                                , parseOnly
                                                , satisfy
                                                , skipMany
                                                , word8
                                                )
import qualified Data.ByteString               as BS
import           Data.Word                      ( Word8 )
import           Pdf.Object.Object              ( isWhiteSpace )
import           Util.Ascii                     ( asciiEXCLAMATIONMARK
                                                , asciiGREATERTHANSIGN
                                                , asciiLOWERU
                                                , asciiLOWERZ
                                                , asciiTILDE
                                                )
import           Util.Errors                    ( UnifiedError
                                                  ( InvalidAscii85Stream
                                                  )
                                                )

specialZeroP :: Get BS.ByteString
specialZeroP = do
  skipMany (satisfy isWhiteSpace)
  word8 asciiLOWERZ
  return "\x00\x00\x00\x00"

isAscii85Digit :: Word8 -> Bool
isAscii85Digit byte = byte >= asciiEXCLAMATIONMARK && byte <= asciiLOWERU

a85digitP :: Get Word8
a85digitP = do
  skipMany (satisfy isWhiteSpace)
  satisfy isAscii85Digit

baseN :: Int -> Int -> Int -> [Int]
baseN width base value = reverse $ baseN' width value
 where
  baseN' :: Int -> Int -> [Int]
  baseN' 0 _ = []
  baseN' width' value' =
    let (q, r) = divMod value' base in r : baseN' (width' - 1) q

base256ToBase85 :: Word8 -> Word8 -> Word8 -> Word8 -> Get BS.ByteString
base256ToBase85 b1 b2 b3 b4 =
  let b1' = fromIntegral b1 :: Int
      b2' = fromIntegral b2 :: Int
      b3' = fromIntegral b3 :: Int
      b4' = fromIntegral b4 :: Int
      c   = b1' * 16777216 + b2' * 65536 + b3' * 256 + b4'
  in  return
        . BS.pack
        . fmap (+ asciiEXCLAMATIONMARK)
        . fmap fromIntegral
        $ baseN 5 85 c

base85ToBase256
  :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Get BS.ByteString
base85ToBase256 c1 c2 c3 c4 c5 =
  let c1' = fromIntegral (c1 - asciiEXCLAMATIONMARK) :: Int
      c2' = fromIntegral (c2 - asciiEXCLAMATIONMARK) :: Int
      c3' = fromIntegral (c3 - asciiEXCLAMATIONMARK) :: Int
      c4' = fromIntegral (c4 - asciiEXCLAMATIONMARK) :: Int
      c5' = fromIntegral (c5 - asciiEXCLAMATIONMARK) :: Int
      b   = c1' * 52200625 + c2' * 614125 + c3' * 7225 + c4' * 85 + c5'
  in  if b >= 4294967296
        then fail "Value too big for ASCII85"
        else return . BS.pack . fmap fromIntegral $ baseN 4 256 b

fiveAscii85DigitsP :: Get BS.ByteString
fiveAscii85DigitsP = do
  c1 <- a85digitP
  c2 <- a85digitP
  c3 <- a85digitP
  c4 <- a85digitP
  c5 <- a85digitP
  base85ToBase256 c1 c2 c3 c4 c5

fourAscii85DigitsP :: Get BS.ByteString
fourAscii85DigitsP = do
  c1 <- a85digitP
  c2 <- a85digitP
  c3 <- a85digitP
  c4 <- a85digitP
  BS.take 3 <$> base85ToBase256 c1 c2 c3 c4 0

threeAscii85DigitsP :: Get BS.ByteString
threeAscii85DigitsP = do
  c1 <- a85digitP
  c2 <- a85digitP
  c3 <- a85digitP
  BS.take 2 <$> base85ToBase256 c1 c2 c3 0 0

twoAscii85DigitsP :: Get BS.ByteString
twoAscii85DigitsP = do
  c1 <- a85digitP
  c2 <- a85digitP
  BS.take 1 <$> base85ToBase256 c1 c2 0 0 0

ascii85GroupP :: Get BS.ByteString
ascii85GroupP =
  fiveAscii85DigitsP
    <|> fourAscii85DigitsP
    <|> threeAscii85DigitsP
    <|> twoAscii85DigitsP

endOfDataMarkerP :: Get ()
endOfDataMarkerP = do
  skipMany (satisfy isWhiteSpace)
  word8 asciiTILDE
  word8 asciiGREATERTHANSIGN

decodeAscii85P :: Get BS.ByteString
decodeAscii85P = label "ascii85" $ do
  values <- many' (specialZeroP <|> ascii85GroupP)
  endOfDataMarkerP
  return (BS.concat values)

{-|
Decode an Ascii85 stream.
-}
decode :: BS.ByteString -> Either UnifiedError BS.ByteString
decode stream = case parseOnly (decodeAscii85P <* endOfInput) stream of
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
Encode a bytestring to an Ascii85 stream.
-}
encode :: BS.ByteString -> Either UnifiedError BS.ByteString
encode stream = case parseOnly (encodeAscii85P <* endOfInput) stream of
  Left  msg     -> Left (InvalidAscii85Stream msg)
  Right encoded -> Right (BS.concat [encoded, "~>"])
