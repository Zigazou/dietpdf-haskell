{-|
This module is a frontend to ECT executable.

It allows to handle errors of Zlib stream decoding with the unified errors. As
it uses `Codec.Compression.Zlib.Internal` to be able to handle errors, it is
not guaranteed to be upward compatible with future versions of Zlib.

It works only with strict bytestrings.
-}
module Codec.Compression.ECT
  ( compress
  ) where

import Codec.Compression.GZip qualified as GZip

import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)

import Data.Bits (Bits ((.|.)), shiftL, shiftR, testBit)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Fallible (Fallible, FallibleT)
import Data.Functor ((<&>))
import Data.Word (Word32, Word8)

import External.ExternalCommand (externalCommand)

import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import System.IO.Unsafe (unsafePerformIO)

{-|
Compress a strict bytestring.

It uses the Zopfli algorithm to achieve the best compression possible with the
deflate algorithm.

The output is standardized but will always return a Right value.
-}
compress :: ByteString -> Fallible ByteString
compress input = unsafePerformIO $ runExceptT $ compress' input

compress' :: ByteString -> FallibleT IO ByteString
compress' input =
  withSystemTempFile "dietpdf.gz" $ \gzPath tempFile -> do
    -- Compress the input using GZip to create a temporary GZip file.
    lift $ BS.hPut tempFile (BL.toStrict $ GZip.compress $ BL.fromStrict input)
    _ <- lift $ hClose tempFile

    -- Call the external ECT command to optimize compression of the GZip file.
    externalCommand "ect" ["-9", "-gzip", gzPath]

    -- Read the optimized GZip file and extract the raw deflate stream.
    lift $ BS.readFile gzPath <&> zlibWrap input . extractGzipDeflate

{-|
Extracts the raw deflate stream from a GZip-compressed bytestring.

The GZip format includes headers and footers, so this function calculates the
correct offset to retrieve only the deflate-compressed data.
-}
extractGzipDeflate :: ByteString -> ByteString
extractGzipDeflate gz =
  let off = gzipPayloadOffset gz
      end = BS.length gz - 8
  in BS.take (end - off) (BS.drop off gz)

{-|
Calculates the offset in a GZip-compressed bytestring where the deflate payload
begins.

This function reads the GZip header flags and any optional fields to determine
the correct starting point of the deflate-compressed data.
-}
gzipPayloadOffset :: ByteString -> Int
gzipPayloadOffset gz =
  let flg = BS.index gz 3
      base = 10 :: Int
      afterExtra i =
        if testBit flg 2
          then
            let xlen = fromIntegral (BS.index gz i)
                    + 256 * fromIntegral (BS.index gz (i + 1))
            in i + 2 + xlen
          else i
      skipZero i =
        case BS.elemIndex 0 (BS.drop i gz) of
          Just n  -> i + n + 1
          Nothing -> i
      i1 = afterExtra base
      i2 = if testBit flg 3 then skipZero i1 else i1
      i3 = if testBit flg 4 then skipZero i2 else i2
      i4 = if testBit flg 1 then i3 + 2 else i3
  in i4

{-|
Wraps a raw deflate stream with a zlib header and Adler-32 checksum.

This function takes the original uncompressed data and the raw
deflate-compressed data, then constructs a zlib-compliant byte sequence.
-}
zlibWrap :: ByteString -> ByteString -> ByteString
zlibWrap original rawDeflate =
  BS.concat
    [ BS.pack [0x78, 0xDA]
    , rawDeflate
    , word32be (adler32 original)
    ]

{-|
Converts a Word32 value to a big-endian ByteString.
-}
word32be :: Word32 -> ByteString
word32be word =
  BS.pack
    [ fromIntegral (word `shiftR` 24)
    , fromIntegral (word `shiftR` 16)
    , fromIntegral (word `shiftR` 8)
    , fromIntegral word
    ]

{-|
Calculates the Adler-32 checksum of a bytestring.

This checksum is used in the zlib format to verify data integrity. The Adler-32
algorithm is a simple checksum algorithm that is faster than CRC32 and is
suitable for use in zlib compression.
-}
adler32 :: ByteString -> Word32
adler32 =
  uncurry combine . BS.foldl' step (1, 0)
 where
  modulo :: Word32
  modulo = 65521

  step :: (Word32, Word32) -> Word8 -> (Word32, Word32)
  step (a, b) x =
    let a' = (a + fromIntegral x) `mod` modulo
        b' = (b + a') `mod` modulo
    in (a', b')

  combine :: Word32 -> Word32 -> Word32
  combine a b = shiftL b 16 .|. a
