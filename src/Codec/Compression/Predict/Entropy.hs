{-|
This module provides entropy heuristics for data prediction.
-}
module Codec.Compression.Predict.Entropy
  ( entropyShannon
  , entropySum
  , entropyLFS
  , Entropy(EntropyDeflate, EntropyShannon, EntropyRLE, EntropySum, EntropyLFS)
  ) where


import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Kind (Type)

{-|
Supported entropy heuristics.
-}
type Entropy :: Type
data Entropy = EntropyShannon
             | EntropyDeflate
             | EntropyRLE
             | EntropySum
             | EntropyLFS
             deriving stock Eq

{-|
Calculate the Shannon entropy of a `ByteString`.
Adapted from https://rosettacode.org/wiki/Entropy
-}
entropyShannon :: ByteString -> Double
entropyShannon =
  sum'
    . map ponderate
    . frequency
    . map (fromIntegral . BS.length)
    . BS.group
    . BS.sort
 where
  sum' :: [Double] -> Double
  sum' = foldr (+) 0.0

  ponderate :: Double -> Double
  ponderate value = -(value * logBase 2 value)

  frequency :: [Double] -> [Double]
  frequency values = let valuesSum = sum' values in map (/ valuesSum) values

{-|
Calculate a simple sum-based entropy of a `ByteString`.
-}
entropySum :: ByteString -> Double
entropySum = BS.foldl' (\acc w -> acc + (fromIntegral w - 128)) 0.0

{-|
Calculate a simple LFS-based entropy of a `ByteString`.
-}
entropyLFS :: ByteString -> Double
entropyLFS =
  (/ 65536.0)
    . fromIntegral
    . sum
    . map ((\n -> n * ilog2i n) . toInteger . BS.length)
    . BS.group
    . BS.sort
 where
  ilog2 :: Integer -> Integer
  ilog2 n
    | n <= 1 = 0
    | otherwise = 1 + ilog2 (n `div` 2)

  {- Integer approximation in Q16.16 of log2(n), inspired by LodePNG helpers
  (an integer part + a linear approximation of the mantissa).
  -}
  ilog2i :: Integer -> Integer
  ilog2i n
    | n <= 1 = 0
    | otherwise =
        let k = ilog2 n
            pow2 = (2 :: Integer) ^ (fromIntegral k :: Int)
            mantissaQ16 = (n * 65536) `div` pow2 -- in [65536, 131071]
            slopeQ16 = 94548 :: Integer -- round(65536 / ln(2))
            fracQ16 = ((mantissaQ16 - 65536) * slopeQ16) `div` 65536
         in k * 65536 + fracQ16