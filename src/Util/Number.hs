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
import           Data.List.Split                ( splitOn )
import           Data.Word                      ( Word8 )
import           Util.Ascii                     ( asciiDIGITZERO )

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
fromNumber number = case splitOn "." (show number) of
  ["0"    , "0"    ] -> "0"
  ["-0"   , "0"    ] -> "0"
  ["-0"   , decimal] -> BS.concat ["-.", fromString decimal]
  ["0"    , decimal] -> BS.concat [".", fromString decimal]
  [integer, "0"    ] -> fromString integer
  _anyOtherResult    -> fromString (show number)

-- | Output an int
fromInt :: Int -> BS.ByteString
fromInt = fromString . show
