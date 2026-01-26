{-|
Numeric helpers for PDF formatting and decoding.

This module contains small helpers for working with numbers in the context of
PDF serialization:

- formatting floating point numbers in a compact PDF-friendly form;
- parsing ASCII digit sequences into integers;
- encoding positive integers into big-endian byte sequences.
-}
module Util.Number
  ( fromNumber
  , fromInt
  , toNumber
  , bytesNeededToEncode
  , encodeIntToBytes
  , round'
  ) where

import Data.Bits (shiftR)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.UTF8 (fromString)
import Data.Double.Conversion.ByteString (toShortest)
import Data.List (foldl')
import Data.Word (Word8)

import Util.Ascii (asciiDIGITZERO, asciiHYPHENMINUS)
import Util.String (startsWith)

{-|
Rounds a number to a given number of decimal places.

The number of decimal places is given by the first argument.
-}
round' :: Int -> Double -> Double
round' limit x = fromIntegral (floor (x * t + 0.5) :: Int) / t
 where
  t = 10 ^ limit

{-|
Given a list of `Word8`, returns a number.

The list must contain only digit numbers.

The function does no check.
-}
toNumber :: [Word8] -> Int
toNumber = foldl' (\x y -> x * 10 + fromIntegral (y - asciiDIGITZERO)) 0

{-|
Output an optimized floating number for a PDF file.

For example:

- 0.0 → "0"
- -0.0 → "0"
- -0.3 → "-.3"
- 0.3 → ".3"
- 3.0 → "3"
-}
fromNumber :: Double -> ByteString
fromNumber number
  | startsWith "0." str  = BS.drop 1 str
  | startsWith "-0." str = BS.cons asciiHYPHENMINUS (BS.drop 2 str)
  | otherwise            = str
  where str = toShortest (round' 6 number)

{-|
Convert an 'Int' to its decimal ASCII representation.

This is a small convenience wrapper around 'show'.
-}
fromInt :: Int -> ByteString
fromInt = fromString . show

{-|
Calculates the number of bytes needed to encode a number in binary.

This function only works for positive numbers.
-}
bytesNeededToEncode :: Int -> Int
bytesNeededToEncode 0 = 1
bytesNeededToEncode number =
  floor (logBase 256 (fromIntegral number :: Double)) + 1

{-|
Encodes a number in the minimum bytes required.
-}
encodeIntToBytes :: Int -> Int -> ByteString
encodeIntToBytes count number
  | count == 0 = ""
  | otherwise = BS.snoc (encodeIntToBytes (count - 1) leftPart) rightPart
  where
    leftPart :: Int
    leftPart = shiftR number 8

    rightPart :: Word8
    rightPart = fromIntegral number
