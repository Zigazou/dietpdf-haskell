{-|
This module is a frontend to Brotli modules.

It allows to handle errors of Brötli stream decoding with the unified errors.

It works only with strict bytestrings.
-}
module Codec.Compression.BrotliForPDF
  ( decompress
  , compress
  , textCompress
  , fontCompress
  , fastCompress
  , entropyCompress
  ) where

import Codec.Compression.Brotli qualified as BR

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Fallible (Fallible)

brotliCompressOptions :: BR.CompressParams
brotliCompressOptions = BR.defaultCompressParams
  { BR.compressLevel      = BR.CompressionLevel11
  , BR.compressWindowSize = BR.CompressionWindowBits24
  , BR.compressMode       = BR.CompressionModeGeneric
  , BR.compressSizeHint   = 0
  }

brotliEntropyCompressParams :: BR.CompressParams
brotliEntropyCompressParams = brotliCompressOptions
  { BR.compressLevel = BR.CompressionLevel2
  }

brotliFontCompressParams :: BR.CompressParams
brotliFontCompressParams = brotliCompressOptions
  { BR.compressMode = BR.CompressionModeFont
  }

brotliTextCompressParams :: BR.CompressParams
brotliTextCompressParams = brotliCompressOptions
  { BR.compressMode = BR.CompressionModeText
  }

{-|
Compress a strict bytestring.

It uses the Zopfli algorithm to achieve the best compression possible with the
deflate algorithm.

The output is standardized but will always return a Right value.
-}
compress
  :: ByteString -- ^ A strict bytestring to encode
  -> Fallible ByteString -- ^ Either an error or the compressed bytestring
compress content = Right
                 . BL.toStrict
                 . BR.compressWith params
                 . BL.fromStrict
                 $ content
 where
  params = brotliCompressOptions
    { BR.compressSizeHint = fromIntegral (BS.length content) }

{-|
Compress a strict bytestring.

It uses the Brotli algorithm optimized for text data.
-}

textCompress
  :: ByteString -- ^ A strict bytestring to encode
  -> Fallible ByteString -- ^ Either an error or the compressed bytestring
textCompress content = Right
                     . BL.toStrict
                     . BR.compressWith params
                     . BL.fromStrict
                     $ content
 where
  params = brotliTextCompressParams
    { BR.compressSizeHint = fromIntegral (BS.length content) }

{-|
Compress a strict bytestring.

It uses the Brotli algorithm optimized for font data.
-}
fontCompress
  :: ByteString -- ^ A strict bytestring to encode
  -> Fallible ByteString -- ^ Either an error or the compressed bytestring
fontCompress content = Right
                     . BL.toStrict
                     . BR.compressWith params
                     . BL.fromStrict
                     $ content
 where
  params = brotliFontCompressParams
    { BR.compressSizeHint = fromIntegral (BS.length content) }

{-|
Compress a strict bytestring.

It uses the Brotli algorithm.

The output is standardized but will always return a Right value.
-}
fastCompress
  :: ByteString -- ^ A strict bytestring to encode
  -> Fallible ByteString -- ^ Either an error or the compressed bytestring
fastCompress =
  Right . BL.toStrict . BR.compressWith brotliCompressOptions . BL.fromStrict

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
    . BR.compressWith brotliEntropyCompressParams
    . BL.fromStrict

{-|
Decompress a strict bytestring compressed using the Brötli algorithm.

It may return errors on unexpected end of string or incorrect value.
-}
decompress
  :: ByteString -- ^ A strict bytestring to encode
  -> Fallible ByteString
  -- ^ Either an error or the compressed bytestring
decompress = Right
           . BL.toStrict
           . BR.decompressWith BR.defaultDecompressParams
           . BL.fromStrict
