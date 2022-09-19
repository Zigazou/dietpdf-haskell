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
  ) where

import qualified Codec.Compression.Hopfli      as HL
import qualified Codec.Compression.Zlib        as ZL
import qualified Codec.Compression.Zlib.Internal
                                               as ZLI
import           Util.Errors                    ( UnifiedError(FlateDecodeError)
                                                )
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BL

zopfliCompressOptions :: HL.CompressOptions
zopfliCompressOptions = HL.CompressOptions { HL.verbose            = HL.NONE
                                           , HL.numIterations      = 31
                                           , HL.blockSplitting     = True
                                           , HL.blockSplittingLast = False
                                           , HL.blockSplittingMax  = 15
                                           }

{-|
Compress a strict bytestring.

It uses the Zopfli algorithm to achieve the best compression possible with the
deflate algorithm.

The output is standardized but will always return a Right value.
-}
compress
  :: BS.ByteString -- ^ A strict bytestring to encode
  -> Either UnifiedError BS.ByteString
  -- ^ Either an error or the compressed bytestring
compress = Right . HL.compressWith zopfliCompressOptions HL.ZLIB

zlibCompressParams :: ZL.CompressParams
zlibCompressParams =
  ZL.defaultCompressParams { ZL.compressLevel = ZL.bestCompression }

{-|
Compress a strict bytestring.

It uses the standard Zlib algorithm to achieve standard compression while being
fast.

The output is standardized but will always return a Right value.
-}
fastCompress
  :: BS.ByteString -- ^ A strict bytestring to encode
  -> Either UnifiedError BS.ByteString
  -- ^ Either an error or the compressed bytestring
fastCompress =
  Right . BL.toStrict . ZL.compressWith zlibCompressParams . BL.fromStrict

{-|
Decompress a strict bytestring compressed using the Zlib format of the deflate
algorithm.

It may return errors on unexpected end of string or incorrect value.
-}
decompress
  :: BS.ByteString -- ^ A strict bytestring to encode
  -> Either UnifiedError BS.ByteString
  -- ^ Either an error or the compressed bytestring
decompress = fmap BL.toStrict . decompressLazy . BL.fromStrict
 where
  decompressLazy :: BL.ByteString -> Either UnifiedError BL.ByteString
  decompressLazy = ZLI.foldDecompressStreamWithInput
    (fmap . BL.append . BL.fromStrict)
    (const (Right BL.empty))
    (Left . FlateDecodeError . show)
    (ZLI.decompressST ZLI.zlibFormat ZLI.defaultDecompressParams)
