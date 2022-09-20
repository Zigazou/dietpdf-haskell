{-# LANGUAGE OverloadedStrings #-}
-- | 
module Codec.Filter.Ascii85
  () where

import           Control.Applicative            ( (<|>) )
import           Control.Monad.Fail             ( fail )
import           Data.Binary.Parser             ( Get
                                                , label
                                                , many'
                                                , satisfy
                                                , sepBy
                                                , skipMany
                                                , word8
                                                )
import           Data.Bits                      ( (.&.)
                                                , shiftL
                                                , shiftR
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
  digit <- satisfy isAscii85Digit
  return (digit - asciiEXCLAMATIONMARK)

base85ToBase256
  :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Get BS.ByteString
base85ToBase256 c1 c2 c3 c4 c5 =
  let value =
        (fromIntegral c1)
        * 85
        ^ 4
        + (fromIntegral c2)
        * 85
        ^ 3
        + (fromIntegral c3)
        * 85
        ^ 2
        + (fromIntegral c4)
        * 85
        + (fromIntegral c5) :: Int
  in  if value >= 2 ^ 32
        then fail "Value too big for ASCII85"
        else
          return
          . BS.pack
          . fmap fromIntegral
          $ [ shiftR (value .&. 0x000000ff000000) 24
            , shiftR (value .&. 0x00000000ff0000) 16
            , shiftR (value .&. 0x0000000000ff00) 8
            , shiftR (value .&. 0x000000000000ff) 0
            ]

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
  base85ToBase256 c1 c2 c3 c4 0

threeAscii85DigitsP :: Get BS.ByteString
threeAscii85DigitsP = do
  c1 <- a85digitP
  c2 <- a85digitP
  c3 <- a85digitP
  base85ToBase256 c1 c2 c3 0 0

twoAscii85DigitsP :: Get BS.ByteString
twoAscii85DigitsP = do
  c1 <- a85digitP
  c2 <- a85digitP
  base85ToBase256 c1 c2 0 0 0

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

{-|
A binary parser for an Ascii 85 encoded string.
-}
ascii85P :: Get BS.ByteString
ascii85P = label "ascii85" $ do
  values <- many' (ascii85GroupP <|> specialZeroP)
  endOfDataMarkerP
  return (BS.concat values)
