module Codec.Compression.Predict.TIFF
        ( tiff_predict_row
        , tiffPredictRow
        , tiffUnpredictRow
        ) where

import Data.Bitmap.BitmapConfiguration
  (BitmapConfiguration (bcBitsPerComponent, bcComponents, bcLineWidth))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.Word (Word32, Word8)

import Foreign.C.Types (CSize (CSize))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Array (pokeArray)
import Foreign.Ptr (Ptr, castPtr)

import System.IO.Unsafe (unsafePerformIO)

-- FFI binding to the C implementation in cbits/tiff_predict.c
foreign import ccall unsafe "tiff_predict_row_ffi"
  tiff_predict_row
    :: Ptr Word8
    -> CSize
    -> Word32
    -> Word8
    -> Word8
    -> IO ()

-- FFI binding to the C implementation in cbits/tiff_predict.c
foreign import ccall unsafe "tiff_unpredict_row_ffi"
  tiff_unpredict_row
    :: Ptr Word8
    -> CSize
    -> Word32
    -> Word8
    -> Word8
    -> IO ()

{-|
Apply TIFF Predictor 2 to a scanline in Haskell-friendly form.

Arguments:
- row bytes (packed as provided: 2/4/8/16 bpc)
- image width (pixels)
- bits per component (2, 4, 8, or 16)
- color components per pixel (1–4)

Returns a new ByteString with prediction applied (in-place on a copy).
-}
tiffPredictRow
  :: BitmapConfiguration  -- ^ image width (pixels)
  -> ByteString           -- ^ row bytes
  -> ByteString           -- ^ predicted row bytes
tiffPredictRow bitmapConfig row = unsafePerformIO $ do
  let
    rowLength = BS.length row
    width = bcLineWidth bitmapConfig
    bitsPerComponent = bcBitsPerComponent bitmapConfig
    colorComponents = bcComponents bitmapConfig

  output <- BSI.mallocByteString rowLength

  withForeignPtr output $ \outPtr -> do
    -- Copy input row into mutable buffer we can modify in-place
    pokeArray (castPtr outPtr) (BS.unpack row)

    -- Call the FFI predictor
    tiff_predict_row
      (castPtr outPtr)
      (fromIntegral rowLength :: CSize)
      (fromIntegral width :: Word32)
      (fromIntegral $ fromEnum bitsPerComponent :: Word8)
      (fromIntegral colorComponents :: Word8)

    pure $ BSI.PS output 0 rowLength

{-|
Apply TIFF Predictor 2 to a scanline in Haskell-friendly form.

Arguments:
- row bytes (packed as provided: 2/4/8/16 bpc)
- image width (pixels)
- bits per component (2, 4, 8, or 16)
- color components per pixel (1–4)

Returns a new ByteString with prediction applied (in-place on a copy).
-}
tiffUnpredictRow
  :: BitmapConfiguration  -- ^ image width (pixels)
  -> ByteString           -- ^ row bytes
  -> ByteString           -- ^ predicted row bytes
tiffUnpredictRow bitmapConfig row = unsafePerformIO $ do
  let
    rowLength = BS.length row
    width = bcLineWidth bitmapConfig
    bitsPerComponent = bcBitsPerComponent bitmapConfig
    colorComponents = bcComponents bitmapConfig

  output <- BSI.mallocByteString rowLength

  withForeignPtr output $ \outPtr -> do
    -- Copy input row into mutable buffer we can modify in-place
    pokeArray (castPtr outPtr) (BS.unpack row)

    -- Call the FFI predictor
    tiff_unpredict_row
      (castPtr outPtr)
      (fromIntegral rowLength :: CSize)
      (fromIntegral width :: Word32)
      (fromIntegral $ fromEnum bitsPerComponent :: Word8)
      (fromIntegral colorComponents :: Word8)

    pure $ BSI.PS output 0 rowLength

