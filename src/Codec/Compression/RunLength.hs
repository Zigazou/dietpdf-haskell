{-|
This modules implements the RLE alfgorithm as used in PDF file (and also TIFF)

From Adobe PDF 32000-1:2008:

@
    The RunLengthDecode filter decodes data that has been encoded in a simple
    byte-oriented format based on run length. The encoded data shall be a
    sequence of runs, where each run shall consist of a length byte followed by
    1 to 128 bytes of data. If the length byte is in the range 0 to 127, the
    following length + 1 (1 to 128) bytes shall be copied literally during
    decompression. If length is in the range 129 to 255, the following single
    byte shall be copied 257 - length (2 to 128) times during decompression.
    A length value of 128 shall denote EOD.

    The compression achieved by run-length encoding depends on the input data.
    In the best case (all zeros), a compression of approximately 64 : 1 is
    achieved for long files. The worst case (the hexadecimal sequence 00
    alternating with FF) results in an expansion of 127 : 128.
@
-}
module Codec.Compression.RunLength
  ( compress
  , entropyCompress
  , decompress
  ) where

import Codec.Compression.Flate (fastCompress)

import Control.Monad ((<=<))

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Unsafe qualified as BUS
import Data.Fallible (Fallible)
import Data.UnifiedError (UnifiedError (RLEDecodeError, RLEEncodeError))
import Data.Word (Word8)

import Foreign.C.Types (CSize (CSize))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, castPtr)

import GHC.Base (maxInt)

import System.IO.Unsafe (unsafePerformIO)

{-
FFI to evaluate the size of a run length decompress.
-}
foreign import ccall unsafe "runLengthEvaluateUncompressedSizeFFI"
  c_run_length_evaluate_uncompressed_size :: Ptr Word8 -> CSize -> IO CSize

{-
FFI to run length decompress.
-}
foreign import ccall unsafe "runLengthDecompressFFI"
  c_run_length_decompress :: Ptr Word8 -> CSize -> Ptr Word8 -> IO CSize

{-
FFI to run length compress.
-}
foreign import ccall unsafe "runLengthCompressFFI"
  c_run_length_compress :: Ptr Word8 -> CSize -> Ptr Word8 -> IO CSize

evaluateUncompressedSize :: ByteString -> CSize
evaluateUncompressedSize stream = unsafePerformIO $ do
  BUS.unsafeUseAsCStringLen stream $ \(inputPtr, inputLen) -> do
    c_run_length_evaluate_uncompressed_size
      (castPtr inputPtr)
      (fromIntegral inputLen :: CSize)

{-|
Decode a RLE bytestring.
-}
decompress :: ByteString -> Fallible ByteString
decompress stream =
  unsafePerformIO $ do
    let outputSize = evaluateUncompressedSize stream
    if outputSize < 0
      then pure $ Left (RLEDecodeError "RLE decompression failed")
      else do
        output <- BSI.mallocByteString (fromIntegral outputSize)
        BUS.unsafeUseAsCStringLen stream $ \(inputPtr, inputLen) -> do
          withForeignPtr output $ \outputPtr -> do
            _ <- c_run_length_decompress
              (castPtr inputPtr)
              (fromIntegral inputLen :: CSize)
              outputPtr
            pure ()
        pure $ Right (BSI.PS output 0 (fromIntegral outputSize))

{-|
Encode a bytestring into an RLE bytestring.
-}
compress :: ByteString -> Fallible ByteString
compress stream =
  unsafePerformIO $ do
    let inputLen = BS.length stream
        maxOutputLen = inputLen * 3 `div` 2 + 2
    output <- BSI.mallocByteString maxOutputLen
    outputLen <- BUS.unsafeUseAsCStringLen stream $ \(inputPtr, _) -> do
      withForeignPtr output $ \outputPtr -> do
        c_run_length_compress
          (castPtr inputPtr)
          (fromIntegral inputLen :: CSize)
          outputPtr
    if outputLen == fromIntegral maxOutputLen
      then pure $ Left (RLEEncodeError "RLE compression output size exceeded allocated buffer")
      else pure $ Right (BSI.PS output 0 (fromIntegral outputLen))

{-|
Gives a number showing the "entropy" of a `ByteString`.

The lower the number, the more compressible the `ByteString`.
-}
entropyCompress
  :: ByteString -- ^ A strict bytestring
  -> Double
entropyCompress = fromIntegral
                . either (const maxInt) BS.length
                . (fastCompress <=< compress)
