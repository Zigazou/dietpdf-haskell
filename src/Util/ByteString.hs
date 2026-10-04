{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
ByteString helpers for splitting, transposing, and naming.

This module provides utilities to:

* split raw bytes into fixed-width chunks,
* separate and group interleaved component channels,
* generate compact base names using an alphanumeric digit set.
-}
module Util.ByteString
  ( splitRaw
  , separateComponents
  , groupComponents
  , baseDigits
  , toNameBase
  , containsOnlyGray
  , countGrayLevels
  , compactGray
  , convertToGray
  , optimizeParity
  , cut
  , hexDump
  , HexBS (HexBS)
  , isNearlyGray
  ) where

import Data.Binary (Word8)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Unsafe qualified as BUS
import Data.IntSet qualified as IS
import Data.Kind (Type)
import Data.Map (Map)
import Data.Map.Strict qualified as Map

import Foreign.C.Types (CSize (CSize), CUInt (CUInt))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, castPtr)

import Hexdump (Cfg, defaultCfg, prettyHexCfg, simpleHex, startByte)

import System.IO.Unsafe (unsafePerformIO)

{-
FFI to check if a RGB ByteString contains only gray values.
-}
foreign import ccall unsafe "containsOnlyGrayFFI"
  c_contains_only_gray :: Ptr Word8 -> CSize -> IO Bool

{-
FFI to check if a CbCr ByteString contains only gray values.
-}
foreign import ccall unsafe "isNearlyGrayFFI"
  c_is_nearly_gray :: Ptr Word8 -> Ptr Word8 -> CSize -> IO Bool

{-
FFI to check if a RGB ByteString contains only gray values.
-}
foreign import ccall unsafe "optimizeGrayFFI"
  c_optimize_gray :: Ptr Word8 -> CSize -> Ptr Word8 -> IO CSize

{-
FFI to optimize RGB triplets by adjusting component values to have the same
parity.
-}
foreign import ccall unsafe "optimizeParityFFI"
  c_optimize_parity :: Ptr Word8 -> CSize -> Ptr Word8 -> IO CSize

{-|
Cut a `ByteString` from a specific start position with a given length.
-}
cut :: Int -> Int -> ByteString -> ByteString
cut start len = BS.take len . BS.drop start

{-|
Configuration for hexdump starting at a given offset.
-}
hexCfg :: Int -> Cfg
hexCfg offset = defaultCfg { startByte = offset }

{-|
Display a hexdump of a ByteString starting at a given offset.
-}
hexDump :: Int -> ByteString -> String
hexDump offset bytes = do
  let bytes' = BS.take 256 (BS.drop offset bytes)

  prettyHexCfg (hexCfg offset) bytes'

{-|
The HexBS newtype is used to display ByteStrings in hexadecimal format
for easier comparison in test outputs with HSpec.
-}
type HexBS :: Type
newtype HexBS = HexBS ByteString
  deriving newtype (Eq)

instance Show HexBS where
  show (HexBS bs) = simpleHex bs

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
groupComponents [raw]   = raw
groupComponents streams = BS.concat (BS.transpose streams)

{-|
Check if a RGB `ByteString` contains only gray values, i.e., components have
equal values.

This is useful to detect when a RGB image can be converted to a grayscale image.
This only works when the input `ByteString` contains a multiple of 3 bytes
(components are 8 bits each).
-}
containsOnlyGray :: ByteString -> Bool
containsOnlyGray rgbRaw = unsafePerformIO $ do
  BUS.unsafeUseAsCStringLen rgbRaw $ \(input, inputLen) -> do
    c_contains_only_gray (castPtr input) (fromIntegral inputLen :: CSize)

{-|
Check if a CbCr `ByteString` contains only gray values, i.e., the chroma
components are nearly zero.
-}
isNearlyGray :: ByteString -> ByteString -> Bool
isNearlyGray cb cr = unsafePerformIO $ do
  BUS.unsafeUseAsCStringLen cb $ \(cbPtr, cbLen) -> do
    BUS.unsafeUseAsCStringLen cr $ \(crPtr, _crLen) -> do
      c_is_nearly_gray (castPtr cbPtr)
                       (castPtr crPtr)
                       (fromIntegral cbLen :: CSize)

{-|
Converts a Grayscale RGB `ByteString` to a Grayscale `ByteString`.

This function assumes that the input `ByteString` contains only gray values,
i.e., all RGB components are (nearly) equal.
-}
convertToGray :: ByteString -> ByteString
convertToGray bs = unsafePerformIO $ do
  let
    len :: Int
    len = BS.length bs

  output <- BSI.mallocByteString len
  outputLen <- BUS.unsafeUseAsCStringLen bs $ \(input, inputLen) -> do
    withForeignPtr output $ \outputPtr -> do
      c_optimize_gray
        (castPtr input)
        (fromIntegral inputLen :: CSize)
        outputPtr

  pure $ BSI.PS output 0 (fromIntegral outputLen)

{-|
Optimize RGB triplets by adjusting component values to have the same parity.
-}
optimizeParity :: ByteString -> ByteString
optimizeParity bs = unsafePerformIO $ do
  let
    len :: Int
    len = BS.length bs

  output <- BSI.mallocByteString len
  outputLen <- BUS.unsafeUseAsCStringLen bs $ \(input, inputLen) -> do
    withForeignPtr output $ \outputPtr -> do
      c_optimize_parity
        (castPtr input)
        (fromIntegral inputLen :: CSize)
        outputPtr

  pure $ BSI.PS output 0 (fromIntegral outputLen)

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
      let
        quotient :: Int
        remainder :: Int
        (quotient, remainder) = n `divMod` BS.length baseDigits
      in
        toNameBase' quotient (BS.index baseDigits remainder `BS.cons` acc)

-- | Count distinct 8-bit grayscale samples.
countGrayLevels :: ByteString -> Int
countGrayLevels bytes = unsafePerformIO $
  BUS.unsafeUseAsCStringLen bytes $ \(input, inputLen) ->
    fromIntegral <$> c_count_gray_levels (castPtr input) (fromIntegral inputLen)

foreign import ccall unsafe "countGrayLevelsFFI"
  c_count_gray_levels :: Ptr Word8 -> CSize -> IO CSize

foreign import ccall unsafe "packGrayFFI"
  c_pack_gray
    :: Ptr Word8
    -> CSize
    -> CSize
    -> CUInt
    -> Ptr Word8
    -> Ptr Word8
    -> IO CSize

-- | Losslessly compact grayscale rows. The optional palette contains gray bytes
-- for an Indexed /DeviceGray color space; Nothing means direct gray samples.
compactGray
  :: Int
  -> Int
  -> ByteString
  -> Maybe (Int, Maybe ByteString, ByteString)
compactGray width height bytes
  | width <= 0
  || height <= 0
  || width > maxBound `quot` height
  || BS.length bytes /= width * height
  = Nothing

  | bits == 8
  = Just (8, Nothing, bytes)

  | otherwise
  = Just (bits, if direct then Nothing else Just palette, packed)
 where
  -- Count the number of distinct grayscale levels in the input bytes.
  count :: Int
  count = countGrayLevels bytes

  -- Count the number of distinct grayscale levels in the input bytes.
  bits :: Int
  bits | count <= 2  = 1
       | count <= 4  = 2
       | count <= 16 = 4
       | otherwise   = 8

  -- Extract the distinct grayscale levels from the input bytes.
  levels :: [Int]
  levels = IS.toAscList
    (BS.foldl'
      (\seen value -> IS.insert (fromIntegral value) seen)
      IS.empty
      bytes
    )

  -- Create the palette for the reduced set of grayscale levels.
  palette :: ByteString
  palette = BS.pack (map fromIntegral levels)

  -- Calculate the step size for mapping grayscale values to the reduced set of
  -- levels.
  step :: Int
  step = 255 `quot` (2 ^ bits - 1)

  -- Determine if the grayscale values can be represented directly without a
  -- palette.
  direct :: Bool
  direct = all (\value -> value `rem` step == 0) levels

  -- Create a mapping from grayscale values to their corresponding indices in
  -- the palette.
  indices :: Map Int Int
  indices = Map.fromList (zip levels [0 :: Int ..])

  -- Create a lookup table for mapping grayscale values to their packed
  -- representation.
  lookupBytes :: ByteString
  lookupBytes = BS.pack
    [ fromIntegral (if direct
                      then value `quot` step
                      else Map.findWithDefault 0 value indices)
    | value <- [0 .. 255]
    ]

  -- Calculate the stride (number of bytes per row) for the packed grayscale
  -- data.
  stride :: Int
  stride = width `quot` (8 `quot` bits)
         + if width `rem` (8 `quot` bits) == 0 then 0 else 1

  -- Create the packed grayscale data using the C FFI function.
  packed :: ByteString
  packed = unsafePerformIO $
    BUS.unsafeUseAsCString bytes $ \input ->
      BUS.unsafeUseAsCString lookupBytes $ \lookupPtr ->
        BSI.createAndTrim
          (stride * height)
          ( fmap fromIntegral
          . c_pack_gray
              (castPtr input)
              (fromIntegral width)
              (fromIntegral height)
              (fromIntegral bits)
              (castPtr lookupPtr)
          )
