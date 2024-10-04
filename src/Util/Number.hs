
-- | This modules gives helper functions for working with numbers.
module Util.Number
  ( fromNumber
  , fromNumberRounded
  , fromInt
  , toNumber
  , bytesNeededToEncode
  , encodeIntToBytes
  , round'
  , roundAndAHalf
  ) where

import Data.Bits (shiftR)
import Data.ByteString qualified as BS
import Data.ByteString.UTF8 (fromString)
import Data.Double.Conversion.ByteString (toShortest)
import Data.List (foldl')
import Data.Word (Word8)

import Util.Ascii (asciiDIGITZERO, asciiHYPHENMINUS)
import Util.String (startsWith)

{- |
Rounds a number to a given number of decimal places.

The number of decimal places is given by the first argument.
-}
round' :: Int -> Double -> Double
round' limit x = fromIntegral (floor (x * t + 0.5) :: Int) / t
 where
  t = 10 ^ limit

{- |
Rounds a number to a given number of decimal places.

The number of decimal places is given by the first argument.
-}
roundAndAHalf :: Int -> Double -> Double
roundAndAHalf limit x = adjusted
 where
  rounded    = round' limit x
  maxDelta   = 0.25 * (10 ** fromIntegral (-limit))
  adjustment = 0.5 * (10 ** fromIntegral (-limit))
  adjusted
    | x - rounded >= maxDelta    = round' 12 (rounded + adjustment)
    | x - rounded <= (-maxDelta) = round' 12 (rounded - adjustment)
    | otherwise                  = rounded

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
fromNumber :: Double -> BS.ByteString
fromNumber number
  | startsWith "0." str  = BS.drop 1 str
  | startsWith "-0." str = BS.cons asciiHYPHENMINUS (BS.drop 2 str)
  | otherwise            = str
  where str = toShortest (round' 6 number)

{-|
Output an optimized floating number for a PDF file.

For example:

- 0.0 → "0"
- -0.0 → "0"
- -0.3 → "-.3"
- 0.3 → ".3"
- 3.0 → "3"
-}
fromNumberRounded :: Int -> Double -> BS.ByteString
fromNumberRounded limit number
  | startsWith "0." str  = BS.drop 1 str
  | startsWith "-0." str = BS.cons asciiHYPHENMINUS (BS.drop 2 str)
  | otherwise            = str
  where str = toShortest (round' limit number)

-- | Output an int
fromInt :: Int -> BS.ByteString
fromInt = fromString . show

{- |
Calculates the number of bytes needed to encode a number in binary.

This function only works for positive numbers.
-}
bytesNeededToEncode :: Int -> Int
bytesNeededToEncode 0 = 1
bytesNeededToEncode number =
  floor (logBase 256 (fromIntegral number :: Double)) + 1

{- |
Encodes a number in the minimum bytes required.
-}
encodeIntToBytes :: Int -> Int -> BS.ByteString
encodeIntToBytes count number
  | count == 0 = ""
  | otherwise = BS.snoc (encodeIntToBytes (count - 1) leftPart) rightPart
  where
    leftPart :: Int
    leftPart = shiftR number 8

    rightPart :: Word8
    rightPart = fromIntegral number
