-- | This modules contains functions to help manipulating of ByteString.
module Util.ByteString
  ( splitRaw
  , separateComponents
  , groupComponents
  , sndLengthCompare
  ) where

import Data.ByteString qualified as BS

{- |
Split a `ByteString` in `ByteString` of specific length.
-}
splitRaw :: Int -> BS.ByteString -> [BS.ByteString]
splitRaw width = splitRaw'
 where
  splitRaw' raw | BS.length chunk == 0 = []
                | otherwise            = chunk : splitRaw' remain
    where (chunk, remain) = BS.splitAt width raw

{- |
Divide a `ByteString` into `List` of (color) components.

>>> separateComponents 3 "ABCDEFGHIJKLMNO"
["ADGJM", "BEHKN", "CFILO"]
-}
separateComponents :: Int -> BS.ByteString -> [BS.ByteString]
separateComponents 1 raw          = [raw]
separateComponents components raw = BS.transpose (splitRaw components raw)

{- |
Group a `List` of `ByteString` (color components) into a `ByteString`.

>>> groupComponents ["ADGJM", "BEHKN", "CFILO"]
"ABCDEFGHIJKLMNO"
-}
groupComponents :: [BS.ByteString] -> BS.ByteString
groupComponents = BS.concat . BS.transpose

{- |
Compare lengths of bytestrings in second position of couples.
-}
sndLengthCompare :: (a, BS.ByteString) -> (a, BS.ByteString) -> Ordering
sndLengthCompare (_, x) (_, y) = compare (BS.length x) (BS.length y)
