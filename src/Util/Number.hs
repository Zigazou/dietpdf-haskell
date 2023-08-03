
-- | This modules gives helper functions for working with numbers.
module Util.Number
  ( fromNumber
  , fromInt
  , toNumber
  , bytesNeededToEncode
  , encodeIntToBytes
  ) where

import qualified Data.ByteString               as BS
import           Data.ByteString.UTF8           ( fromString )
import           Data.List                      ( foldl' )
import           Data.Word                      ( Word8 )
import           Util.Ascii                     ( asciiDIGITZERO
                                                , asciiHYPHENMINUS
                                                )
import           Data.Double.Conversion.ByteString
                                                ( toShortest )
import           Util.String                    ( startsWith )
import           Data.Bits                      ( shiftR )

round' :: Int -> Double -> Double
round' limit x = fromIntegral (round (x * t) :: Int) / t
 where
  t :: Double
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
fromNumber :: Double -> BS.ByteString
fromNumber number
  | startsWith "0." str  = BS.drop 1 str
  | startsWith "-0." str = BS.cons asciiHYPHENMINUS (BS.drop 2 str)
  | otherwise            = str
  where str = toShortest (round' 3 number)

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
