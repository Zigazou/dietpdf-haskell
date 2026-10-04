{-|
This module provides entropy heuristics for data prediction.
-}
module Codec.Compression.Predict.Entropy
  ( entropyShannon
  , entropySum
  , entropyLFS
  , entropyMSAD
  , Entropy
      ( EntropyDeflate
      , EntropyShannon
      , EntropyRLE
      , EntropySum
      , EntropyLFS
      , EntropyMSAD
      )
  ) where


import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Kind (Type)
import Data.Word (Word8)

{-|
Supported entropy heuristics.
-}
type Entropy :: Type
data Entropy = EntropyShannon -- ^ Shannon entropy
             | EntropyDeflate -- ^ Deflate-based entropy
             | EntropyRLE -- ^ RLE-based entropy
             | EntropySum -- ^ Simple sum-based entropy
             | EntropyLFS -- ^ Simple LFS-based entropy
             | EntropyMSAD -- ^ Minimum sum of absolute differences (libpng)
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
  frequency values =
    let
      valuesSum :: Double
      valuesSum = sum' values
    in
      map (/ valuesSum) values

{-|
Calculate a simple sum-based entropy of a `ByteString`.
-}
entropySum :: ByteString -> Double
entropySum = BS.foldl' (\acc w -> acc + (fromIntegral w - 128)) 0.0

{-|
Calculate the "minimum sum of absolute differences" heuristic used by
libpng/zlib-ng to pick a scanline filter: each byte is read as a signed delta
(the smaller of the value and 256 minus the value) and the deltas are summed.

This is a single linear pass over the row with no sorting and no actual
compression call, making it far cheaper than 'entropyShannon' or
'FL.entropyCompress' while correlating just as well (if not better) with the
final Deflate size, since it directly measures how close the predicted bytes are
to zero.
-}
entropyMSAD :: ByteString -> Double
entropyMSAD = BS.foldl' (\acc w -> acc + fromIntegral (signedAbs w)) 0.0
 where
  signedAbs :: Word8 -> Int
  signedAbs w =
    let
      v :: Int
      v = fromIntegral w
    in
      min v (256 - v)

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
    | n <= 1
    = 0

    | otherwise
    = let
        k :: Integer
        k = ilog2 n

        pow2 :: Integer
        pow2 = (2 :: Integer) ^ (fromIntegral k :: Int)

        mantissaQ16 :: Integer
        mantissaQ16 = (n * 65536) `div` pow2 -- in [65536, 131071]

        slopeQ16 :: Integer
        slopeQ16 = 94548 :: Integer -- round(65536 / ln(2))

        fracQ16 :: Integer
        fracQ16 = ((mantissaQ16 - 65536) * slopeQ16) `div` 65536
      in
        k * 65536 + fracQ16