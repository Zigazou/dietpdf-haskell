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
  , containsOnlyGray
  , convertToGray
  , optimizeParity
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (byteString, toLazyByteString, word8)
import Data.ByteString.Lazy qualified as BL
import Data.Binary (Word8)
import Data.Kind (Type)

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
Check if a RGB triplet is nearly gray (all components close in value).
-}
isNearlyGray :: Word8 -> Word8 -> Word8 -> Bool
isNearlyGray red green blue =
     abs (red'   - green') < 3
  && abs (green' - blue' ) < 3
  && abs (blue'  - red'  ) < 3
 where
  red', green', blue' :: Int
  red' = fromIntegral red
  green' = fromIntegral green
  blue' = fromIntegral blue

type Parity :: Type
data Parity = Even | Odd deriving stock (Eq)

{-|
Modify a RGB triplet so that all components have the same parity (even/odd).
The parity is taken from the most used parity among the three components.
-}
sameParity :: Word8 -> Word8 -> Word8 -> (Word8, Word8, Word8)
sameParity red green blue =
  case (redParity, greenParity, blueParity) of
    -- Nothing to do: all have same parity
    (Even, Even, Even) -> (red, green, blue)
    (Odd,  Odd,  Odd)  -> (red, green, blue)

    -- Two have even parity: adjust the odd one
    (Even, Even, Odd)  -> (red, green, makeEven blue)
    (Even, Odd,  Even) -> (red, makeEven green, blue)
    (Odd,  Even, Even) -> (makeEven red, green, blue)

    -- Two have odd parity: adjust the even one
    (Odd,  Odd,  Even) -> (red, green, makeOdd blue)
    (Odd,  Even, Odd)  -> (red, makeOdd green, blue)
    (Even, Odd,  Odd)  -> (makeOdd red, green, blue)
 where
  getParity :: Word8 -> Parity
  getParity x = if even x then Even else Odd

  redParity, greenParity, blueParity :: Parity
  redParity   = getParity red
  greenParity = getParity green
  blueParity  = getParity blue

  makeEven :: Word8 -> Word8
  makeEven x = if even x then x else x - 1

  makeOdd :: Word8 -> Word8
  makeOdd x = if odd x then x else x + 1

{-|
Modify all triplets in a `ByteString` using a transformer function.
-}
modifyTriplets :: (Word8 -> Word8 -> Word8 -> (Word8, Word8, Word8)) -> ByteString -> ByteString
modifyTriplets transformer bs = BL.toStrict (toLazyByteString (go 0 len))
 where
  len = BS.length bs
  go offset remaining
    | remaining == 0 = mempty
    | remaining < 3 = byteString (BS.drop offset bs)
    | otherwise =
        let red = BS.index bs offset
            green = BS.index bs (offset + 1)
            blue = BS.index bs (offset + 2)
            (newRed, newGreen, newBlue) = transformer red green blue
        in word8 newRed <> word8 newGreen <> word8 newBlue <> go (offset + 3) (remaining - 3)

{-|
Verify all triplets in a `ByteString` using a verifier function.
-}
tripletsVerify :: (Word8 -> Word8 -> Word8 -> Bool) -> ByteString -> Bool
tripletsVerify verifier = go
 where
  go bs | BS.length bs == 0 = True
        | BS.length bs < 3  = False
        | otherwise         = case BS.unpack triplet of
            [red, green, blue] -> verifier red green blue && go remain
            _anyOtherCase -> False
    where (triplet, remain) = BS.splitAt 3 bs

{-|
Check if a RGB `ByteString` contains only gray values, i.e., components have
equal values.

This is useful to detect when a RGB image can be converted to a grayscale image.
This only works when the input `ByteString` contains a multiple of 3 bytes
(components are 8 bits each).
-}
containsOnlyGray :: ByteString -> Bool
containsOnlyGray rgbRaw =
  (BS.length rgbRaw `mod` 3) == 0 && tripletsVerify isNearlyGray rgbRaw

{-|
Converts a Grayscale RGB `ByteString` to a Grayscale `ByteString`.

This function assumes that the input `ByteString` contains only gray values,
i.e., all RGB components are (nearly) equal.
-}
convertToGray :: ByteString -> ByteString
convertToGray rgbRaw = if BS.length rgbRaw `mod` 3 /= 0
  then
    rgbRaw
  else
    case separateComponents 3 rgbRaw of
      [rComponents, _gComponents, _bComponents] -> rComponents
      _anyOtherCase -> rgbRaw

{-|
Optimize RGB triplets by adjusting component values to have the same parity.
-}
optimizeParity :: ByteString -> ByteString
optimizeParity = modifyTriplets sameParity

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
