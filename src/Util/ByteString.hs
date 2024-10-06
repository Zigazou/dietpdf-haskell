-- | This modules contains functions to help manipulating of ByteString.
module Util.ByteString
  ( splitRaw
  , separateComponents
  , groupComponents
  , sndLengthCompare
  , toNameBase
  , renameStrings
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.List (sortBy)
import Data.List.Extra (nubOrd)
import Data.TranslationTable (TranslationTable, mkTranslationTable)

{- |
Split a `ByteString` in `ByteString` of specific length.
-}
splitRaw :: Int -> ByteString -> [ByteString]
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
separateComponents :: Int -> ByteString -> [ByteString]
separateComponents 1 raw          = [raw]
separateComponents components raw = BS.transpose (splitRaw components raw)

{- |
Group a `List` of `ByteString` (color components) into a `ByteString`.

>>> groupComponents ["ADGJM", "BEHKN", "CFILO"]
"ABCDEFGHIJKLMNO"
-}
groupComponents :: [ByteString] -> ByteString
groupComponents = BS.concat . BS.transpose

{- |
Compare lengths of bytestrings in second position of couples.
-}
sndLengthCompare :: (a, ByteString) -> (a, ByteString) -> Ordering
sndLengthCompare (_, x) (_, y) = compare (BS.length x) (BS.length y)

baseDigits :: ByteString
baseDigits = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

toNameBase :: Int -> ByteString
toNameBase value = toNameBase' value ""
  where
    toNameBase' :: Int -> ByteString -> ByteString
    toNameBase' 0 "" = "0"
    toNameBase' 0 acc = acc
    toNameBase' n acc =
      let (quotient, remainder) = n `divMod` BS.length baseDigits
      in toNameBase' quotient (BS.index baseDigits remainder `BS.cons` acc)

{- |
Rename a list of `ByteString`.
The corresponding `Map` is returned.

Shortest `ByteString` are renamed first, giving them the shortest name.
-}
renameStrings :: [ByteString] -> TranslationTable
renameStrings names = mkTranslationTable
                    $ zip ((sortBy shortFirst . nubOrd) names)
                          (fmap toNameBase [0..])
  where
    shortFirst :: ByteString -> ByteString -> Ordering
    shortFirst x y = if BS.length x /= BS.length y
      then compare (BS.length x) (BS.length y)
      else compare x y
