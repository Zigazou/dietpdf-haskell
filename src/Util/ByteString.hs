{-|
ByteString helpers for splitting, transposing, and naming.

This module provides utilities to:

* split raw bytes into fixed-width chunks,
* separate and group interleaved component channels,
* compare by the length of the second element,
* generate compact base names using an alphanumeric digit set.
-}
module Util.ByteString
  ( splitRaw
  , separateComponents
  , groupComponents
  , sndLengthCompare
  , toNameBase
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS

{-|
Split a `ByteString` in `ByteString` of specific length.
-}
splitRaw :: Int -> ByteString -> [ByteString]
splitRaw width = splitRaw'
 where
  splitRaw' raw | BS.length chunk == 0 = []
                | otherwise            = chunk : splitRaw' remain
    where (chunk, remain) = BS.splitAt width raw

{-|
Divide a `ByteString` into `List` of (color) components.

>>> separateComponents 3 "ABCDEFGHIJKLMNO"
["ADGJM", "BEHKN", "CFILO"]
-}
separateComponents :: Int -> ByteString -> [ByteString]
separateComponents 1 raw          = [raw]
separateComponents components raw = BS.transpose (splitRaw components raw)

{-|
Group a `List` of `ByteString` (color components) into a `ByteString`.

>>> groupComponents ["ADGJM", "BEHKN", "CFILO"]
"ABCDEFGHIJKLMNO"
-}
groupComponents :: [ByteString] -> ByteString
groupComponents = BS.concat . BS.transpose

{-|
Compare lengths of bytestrings in second position of couples.
-}
sndLengthCompare :: (a, ByteString) -> (a, ByteString) -> Ordering
sndLengthCompare (_, x) (_, y) = compare (BS.length x) (BS.length y)

{-|
Digit alphabet used by `toNameBase`: 0–9, a–z, A–Z.
-}
baseDigits :: ByteString
baseDigits = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

{-|
Convert a non-negative integer to a compact base name using the
alphanumeric digit set defined by `baseDigits`.

Produces a `ByteString` representation where 0 maps to "0" and other
values are expressed in mixed-radix base of length `BS.length baseDigits`.
-}
toNameBase :: Int -> ByteString
toNameBase value = toNameBase' value ""
  where
    toNameBase' :: Int -> ByteString -> ByteString
    toNameBase' 0 "" = "0"
    toNameBase' 0 acc = acc
    toNameBase' n acc =
      let (quotient, remainder) = n `divMod` BS.length baseDigits
      in toNameBase' quotient (BS.index baseDigits remainder `BS.cons` acc)
