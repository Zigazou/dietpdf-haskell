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
  , convertToGray
  , optimizeParity
  , cut
  , hexDump
  , HexBS (HexBS)
  , unpackBits
  , packBits
  , unpack16BitBE
  , pack16BitBE
  ) where

import Data.Binary (Word16, Word8)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Unsafe qualified as BUS
import Data.Kind (Type)

import Foreign.C.Types (CSize (CSize))
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
groupComponents = BS.concat . BS.transpose

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
Converts a Grayscale RGB `ByteString` to a Grayscale `ByteString`.

This function assumes that the input `ByteString` contains only gray values,
i.e., all RGB components are (nearly) equal.
-}
convertToGray :: ByteString -> ByteString
convertToGray bs = unsafePerformIO $ do
  let len = BS.length bs
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
  let len = BS.length bs
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
      let (quotient, remainder) = n `divMod` BS.length baseDigits
      in toNameBase' quotient (BS.index baseDigits remainder `BS.cons` acc)

{-|
Unpack sub-byte samples from a ByteString.

For 2-bit and 4-bit samples, multiple samples are packed into each byte.
This function unpacks them into individual Word8 values (one sample per byte).

* 2-bit: 4 samples per byte (MSB first)
* 4-bit: 2 samples per byte (MSB first)

For example, a byte 0b11100100 with 2-bit samples unpacks to [3, 2, 1, 0].
-}
unpackBits :: Int -> ByteString -> [Word8]
unpackBits bitsPerSample bs
  | bitsPerSample == 1 = concatMap unpack1Bit (BS.unpack bs)
  | bitsPerSample == 2 = concatMap unpack2Bit (BS.unpack bs)
  | bitsPerSample == 4 = concatMap unpack4Bit (BS.unpack bs)
  | otherwise = BS.unpack bs
  where
    unpack1Bit :: Word8 -> [Word8]
    unpack1Bit byte =
      [ (byte `shiftR` 7) .&. 0x01
      , (byte `shiftR` 6) .&. 0x01
      , (byte `shiftR` 5) .&. 0x01
      , (byte `shiftR` 4) .&. 0x01
      , (byte `shiftR` 3) .&. 0x01
      , (byte `shiftR` 2) .&. 0x01
      , (byte `shiftR` 1) .&. 0x01
      , byte .&. 0x01
      ]

    unpack2Bit :: Word8 -> [Word8]
    unpack2Bit byte =
      [ (byte `shiftR` 6) .&. 0x03
      , (byte `shiftR` 4) .&. 0x03
      , (byte `shiftR` 2) .&. 0x03
      , byte .&. 0x03
      ]

    unpack4Bit :: Word8 -> [Word8]
    unpack4Bit byte =
      [ (byte `shiftR` 4) .&. 0x0F
      , byte .&. 0x0F
      ]

{-|
Pack sub-byte samples back into a ByteString.

Takes a list of Word8 values (one sample per byte) and packs them according
to the specified bits per sample:

* 2-bit: 4 samples per byte (MSB first)
* 4-bit: 2 samples per byte (MSB first)

This is the inverse of `unpackBits`.
-}
packBits :: Int -> [Word8] -> ByteString
packBits bitsPerSample samples
  | bitsPerSample == 1 = BS.pack (pack1Bit samples)
  | bitsPerSample == 2 = BS.pack (pack2Bit samples)
  | bitsPerSample == 4 = BS.pack (pack4Bit samples)
  | otherwise = BS.pack samples
  where
    pack1Bit :: [Word8] -> [Word8]
    pack1Bit [] = []
    pack1Bit samples' = pack1Bit' samples'
      where
        pack1Bit' :: [Word8] -> [Word8]
        pack1Bit' [] = []
        pack1Bit' bits = case splitAt 8 bits of
          (chunk, rest) -> packByte chunk : pack1Bit' rest
          where
            packByte :: [Word8] -> Word8
            packByte [] = 0
            packByte bs = foldr (.|.) 0 $ zipWith (\b idx -> (b .&. 1) `shiftL` (7 - idx)) bs [0..]

    pack2Bit :: [Word8] -> [Word8]
    pack2Bit [] = []
    pack2Bit samples' = pack2Bit' samples'
      where
        pack2Bit' :: [Word8] -> [Word8]
        pack2Bit' [] = []
        pack2Bit' bits = case splitAt 4 bits of
          ([a, b, c, d], rest) -> ((a `shiftL` 6) .|. (b `shiftL` 4) .|. (c `shiftL` 2) .|. d) : pack2Bit' rest
          ([a, b, c], _)       -> [(a `shiftL` 6) .|. (b `shiftL` 4) .|. (c `shiftL` 2)]
          ([a, b], _)          -> [(a `shiftL` 6) .|. (b `shiftL` 4)]
          ([a], _)             -> [a `shiftL` 6]
          (_, _)              -> []

    pack4Bit :: [Word8] -> [Word8]
    pack4Bit [] = []
    pack4Bit samples' = pack4Bit' samples'
      where
        pack4Bit' :: [Word8] -> [Word8]
        pack4Bit' [] = []
        pack4Bit' bits = case splitAt 2 bits of
          ([a, b], rest) -> ((a `shiftL` 4) .|. b) : pack4Bit' rest
          ([a], _)       -> [a `shiftL` 4]
          (_ , _)        -> []

{-|
Unpack 16-bit samples from a ByteString (big-endian).

Each pair of bytes is combined into a Word16 value, MSB first.
For example, bytes [0x12, 0x34] become Word16 0x1234.
-}
unpack16BitBE :: ByteString -> [Word16]
unpack16BitBE bs = unpack16' (BS.unpack bs)
  where
    unpack16' :: [Word8] -> [Word16]
    unpack16' [] = []
    unpack16' [hi] = [fromIntegral hi `shiftL` 8]
    unpack16' (hi:lo:rest) =
      ((fromIntegral hi `shiftL` 8) .|. fromIntegral lo) : unpack16' rest

{-|
Pack 16-bit samples back into a ByteString (big-endian).

Each Word16 value is split into two bytes, MSB first.
For example, Word16 0x1234 becomes bytes [0x12, 0x34].
-}
pack16BitBE :: [Word16] -> ByteString
pack16BitBE samples = BS.pack $ concatMap pack16' samples
  where
    pack16' :: Word16 -> [Word8]
    pack16' w = [fromIntegral (w `shiftR` 8), fromIntegral (w .&. 0xFF)]
