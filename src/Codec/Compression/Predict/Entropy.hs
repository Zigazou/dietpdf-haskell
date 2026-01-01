{-|
This module implements the predictors as specified by the PDF reference.

There are 2 groups:

- TIFF predictors
- PNG predictors

TIFF predictors group only supports type 2 from the TIFF 6.0 specification
(https://www.itu.int/itudoc/itu-t/com16/tiff-fx/docs/tiff6.pdf, page 64).

PNG predictors group supports predictors defined in the RFC 2083
(https://www.rfc-editor.org/rfc/rfc2083.html).

Main difference between TIFF predictors and PNG predictors is that TIFF
predictors is enabled globally for the image while PNG predictors can be
changed on every scanline.
-}
module Codec.Compression.Predict.Entropy
  ( entropyShannon
  , Entropy(EntropyDeflate, EntropyShannon, EntropyRLE)
  ) where


import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Kind (Type)

type Entropy :: Type
data Entropy = EntropyShannon
             | EntropyDeflate
             | EntropyRLE
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
