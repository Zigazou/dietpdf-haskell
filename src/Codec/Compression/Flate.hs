{-|
This module is a frontend to Zlib and Hopfli modules.

It allows to handle errors of Zlib stream decoding with the unified errors. As
it uses `Codec.Compression.Zlib.Internal` to be able to handle errors, it is
not guaranteed to be upward compatible with future versions of Zlib.

It works only with strict bytestrings.
-}
module Codec.Compression.Flate
  ( decompress
  , compress
  , fastCompress
  , noCompress
  , entropyCompress
  ) where

import Codec.Compression.Hopfli qualified as HL
import Codec.Compression.Zlib qualified as ZL
import Codec.Compression.Zlib.Internal qualified as ZLI

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Fallible (Fallible)
import Data.UnifiedError (UnifiedError (FlateDecodeError))

zopfliCompressOptions :: HL.CompressOptions
zopfliCompressOptions = HL.CompressOptions { HL.verbose            = HL.NONE
                                           , HL.numIterations      = 15
                                           , HL.blockSplitting     = True
                                           , HL.blockSplittingLast = False
                                           , HL.blockSplittingMax  = 15
                                           }

variableNumberOfIterations :: Int -> Int
variableNumberOfIterations dataLength
  | dataLength <   2048 = 1023
  | dataLength <   4096 = 511
  | dataLength <   8192 = 255
  | dataLength <  16384 = 127
  | dataLength <  32768 = 63
  | dataLength <  65536 = 31
  | dataLength < 131072 = 15
  | dataLength < 524288 = 7
  | otherwise           = 3

{-|
Compress a strict bytestring.

It uses the Zopfli algorithm to achieve the best compression possible with the
deflate algorithm.

The output is standardized but will always return a Right value.
-}
compress
  :: ByteString -- ^ A strict bytestring to encode
  -> Fallible ByteString -- ^ Either an error or the compressed bytestring
compress content = Right . HL.compressWith adaptiveOptions HL.ZLIB $ content
  where
    adaptiveOptions = zopfliCompressOptions
                        { HL.numIterations = variableNumberOfIterations
                                           $ BS.length content
                        }

zlibCompressParams :: ZL.CompressParams
zlibCompressParams =
  ZL.defaultCompressParams { ZL.compressLevel = ZL.bestCompression }

zlibEntropyCompressParams :: ZL.CompressParams
zlibEntropyCompressParams =
  ZL.defaultCompressParams { ZL.compressLevel = ZL.defaultCompression }

zlibNoCompressParams :: ZL.CompressParams
zlibNoCompressParams =
  ZL.defaultCompressParams { ZL.compressLevel = ZL.noCompression }

{-|
Compress a strict bytestring.

It uses the standard Zlib algorithm to achieve standard compression while being
fast.

The output is standardized but will always return a Right value.
-}
fastCompress
  :: ByteString -- ^ A strict bytestring to encode
  -> Fallible ByteString -- ^ Either an error or the compressed bytestring
fastCompress =
  Right . BL.toStrict . ZL.compressWith zlibCompressParams . BL.fromStrict
{-|
Gives a number showing the "entropy" of a `ByteString`.

The lower the number, the more compressible the `ByteString`.
-}
entropyCompress
  :: ByteString -- ^ A strict bytestring to encode
  -> Double
entropyCompress =
  fromIntegral
    . BL.length
    . ZL.compressWith zlibEntropyCompressParams
    . BL.fromStrict

{-|
Stores a `ByteString` as a Zlib compressed string but with no compression

The output is standardized but will always return a Right value.
-}
noCompress
  :: ByteString -- ^ A strict bytestring to encode
  -> Fallible ByteString -- ^ Either an error or the compressed bytestring
noCompress =
  Right . BL.toStrict . ZL.compressWith zlibNoCompressParams . BL.fromStrict

{-|
Decompress a strict bytestring compressed using the Zlib format of the deflate
algorithm.

It may return errors on unexpected end of string or incorrect value.
-}
decompress
  :: ByteString -- ^ A strict bytestring to encode
  -> Fallible ByteString
  -- ^ Either an error or the compressed bytestring
decompress = fmap BL.toStrict . decompressLazy . BL.fromStrict
 where
  decompressLazy :: BL.ByteString -> Fallible BL.ByteString
  decompressLazy = ZLI.foldDecompressStreamWithInput
    (fmap . BL.append . BL.fromStrict)
    (const (Right BL.empty))
    (Left . FlateDecodeError . show)
    (ZLI.decompressST ZLI.zlibFormat ZLI.defaultDecompressParams)
