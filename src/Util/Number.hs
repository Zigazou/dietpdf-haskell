{-# LANGUAGE OverloadedStrings #-}

-- | This modules gives helper functions for working with numbers.
module Util.Number
  ( fromNumber
  , fromInt
  , toNumber
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

round' :: Int -> Double -> Double
round' limit x = fromIntegral (floor (x * t) :: Int) / t
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
  where str = toShortest (round' 6 number)

-- | Output an int
fromInt :: Int -> BS.ByteString
fromInt = fromString . show
