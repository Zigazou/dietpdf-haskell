{-# LANGUAGE BangPatterns #-}
module Codec.Compression.Predict
  ( predict
  , predictPNGVariants
  , unpredict
  , Entropy (EntropyDeflate, EntropyRLE, EntropyShannon)
  , Predictor (PNGOptimum, PNGAverage, PNGNone, PNGPaeth, PNGSub, PNGUp, TIFFNoPrediction, TIFFPredictor2)
  )
where

import Codec.Compression.Predict.Entropy
  (Entropy (EntropyDeflate, EntropyRLE, EntropyShannon))
import Codec.Compression.Predict.ImageStream
  ( ImageStream
  , fromPredictedStream
  , fromUnpredictedStream
  , packStream
  , predictImageStream
  , predictPNGImageStreams
  , unpredictImageStream
  )
import Codec.Compression.Predict.Predictor
  ( Predictor (PNGAverage, PNGNone, PNGOptimum, PNGPaeth, PNGSub, PNGUp, TIFFNoPrediction, TIFFPredictor2)
  , decodeRowPredictor
  , isPNGGroup
  )
import Codec.Compression.Predict.TIFF (tiffPredictRow, tiffUnpredictRow)

import Data.Bitmap.BitmapConfiguration
  (BitmapConfiguration (bcLineWidth), bitmapPixelBytes, bitmapRawWidth)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Unsafe qualified as BSU
import Data.Fallible (Fallible)
import Data.UnifiedError
  (UnifiedError (InvalidFilterParm, InvalidNumberOfBytes))
import Data.Word (Word8)

import Foreign.Ptr (Ptr)
import Foreign.Storable (peekByteOff, pokeByteOff)

import Util.ByteString (splitRaw)

{-|
Apply a `Predictor` to a `ByteString`, considering its line width.
-}
predict
  :: Entropy -- ^ Entropy type to use
  -> Predictor -- ^ Predictor to be used to encode
  -> BitmapConfiguration -- ^ Bitmap configuration
  -> ByteString -- ^ Stream to encode
  -> Fallible ByteString -- ^ Encoded stream or an error
predict entropy predictor bitmapConfig stream
  | bcLineWidth bitmapConfig < 1 = Left $ InvalidNumberOfBytes 0 0
  | predictor == TIFFPredictor2
  = let
      rows :: [ByteString]
      rows = splitRaw (bitmapRawWidth bitmapConfig) stream

      predictedRows :: [ByteString]
      predictedRows = tiffPredictRow bitmapConfig <$> rows
     in
      return $ BS.concat predictedRows

  | otherwise
  = do
      imgStm <- fromUnpredictedStream bitmapConfig stream
      return $ packStream (predictImageStream entropy predictor imgStm)

-- | Share PNG row predictions across fixed predictors and entropy strategies.
-- Requests must use PNG predictors.
predictPNGVariants
  :: [(Entropy, Predictor)]
  -> BitmapConfiguration
  -> ByteString
  -> Fallible [ByteString]
predictPNGVariants requests config stream
  | bcLineWidth config < 1
  = Left $ InvalidNumberOfBytes 0 0

  | not (all (isPNGGroup . snd) requests)
  = Left $ InvalidFilterParm "predictPNGVariants requires PNG predictors"

  | otherwise
  = predictPNGImageStreams requests <$> fromUnpredictedStream config stream

{-|
Invert the application of a `Predictor` to a `ByteString`, considering its
line width.
-}
unpredict
  :: Predictor -- ^ Predictor (hint in case of a PNG predictor)
  -> BitmapConfiguration -- ^ Bitmap configuration
  -> ByteString -- ^ Stream to decode
  -> Fallible ByteString -- ^ Decoded stream or an error
unpredict predictor bitmapConfig stream
  | bcLineWidth bitmapConfig < 1
  = Left $ InvalidNumberOfBytes 0 0

  | predictor == TIFFNoPrediction
  = Right stream

  | predictor /= TIFFPredictor2
  , rowBytes > 0, pixelBytes > 0, rowBytes < maxBound
  , BS.length stream `mod` (rowBytes + 1) == 0
  = do
      validateRows 0
      return $ decodePNGRows rowBytes pixelBytes stream

  | predictor == TIFFPredictor2
  = let
      rows :: [ByteString]
      rows = splitRaw (bitmapRawWidth bitmapConfig) stream

      unpredictedRows :: [ByteString]
      unpredictedRows = tiffUnpredictRow bitmapConfig <$> rows
    in
      return $ BS.concat unpredictedRows

  | otherwise
  = do
      imgStm <- fromPredictedStream predictor bitmapConfig stream

      let
        unpredicted :: ImageStream
        unpredicted = unpredictImageStream predictor imgStm

      return $ packStream unpredicted

 where
  rowBytes :: Int
  rowBytes = bitmapRawWidth bitmapConfig

  pixelBytes :: Int
  pixelBytes = bitmapPixelBytes bitmapConfig

  validateRows :: Int -> Fallible ()
  validateRows !offset
    | offset >= BS.length stream
    = Right ()

    | otherwise
    = do
        _ <- decodeRowPredictor (BSU.unsafeIndex stream offset)
        validateRows (offset + rowBytes + 1)

-- | Decode complete PNG rows directly into an interleaved output buffer.
-- Row tags and buffer geometry are validated before entering this loop.
decodePNGRows :: Int -> Int -> ByteString -> ByteString
decodePNGRows rowBytes pixelBytes stream =
  BSI.unsafeCreate outputLength $ \dst -> rows dst 0 0
 where
  outputLength :: Int
  outputLength = BS.length stream - BS.length stream `div` (rowBytes + 1)

  rows :: Ptr Word8 -> Int -> Int -> IO ()
  rows dst !source !target
    | target >= outputLength
    = return ()

    | otherwise
    = do
        bytes dst source target (BSU.unsafeIndex stream source) 0
        rows dst (source + rowBytes + 1) (target + rowBytes)

  bytes :: Ptr Word8 -> Int -> Int -> Word8 -> Int -> IO ()
  bytes dst !source !target !tag !column
    | column >= rowBytes
    = return ()

    | otherwise
    = do
        let
          offset :: Int
          offset = target + column

          sample :: Word8
          sample = BSU.unsafeIndex stream (source + 1 + column)

        left <- if column >= pixelBytes && (tag == 1 || tag == 3 || tag == 4)
                  then peekByteOff dst (offset - pixelBytes)
                  else return 0

        above <- if target > 0 && (tag >= 2 && tag <= 4)
                   then peekByteOff dst (offset - rowBytes)
                   else return 0

        upperLeft <- if target > 0 && column >= pixelBytes && tag == 4
                       then peekByteOff dst (offset - rowBytes - pixelBytes)
                       else return 0

        let
          prediction :: Word8
          prediction = case tag of
            1 -> left
            2 -> above
            3 -> fromIntegral (
                  (fromIntegral left + fromIntegral above :: Int) `div` 2
                  )
            4 -> paeth left above upperLeft
            _ -> 0

        pokeByteOff dst offset (sample + prediction)
        bytes dst source target tag (column + 1)

  paeth :: Word8 -> Word8 -> Word8 -> Word8
  paeth left above upperLeft =
    let
      a :: Int
      a = fromIntegral left

      b :: Int
      b = fromIntegral above

      c :: Int
      c = fromIntegral upperLeft

      p :: Int
      p = a + b - c

      da :: Int
      da = abs (p - a)

      db :: Int
      db = abs (p - b)

      dc :: Int
      dc = abs (p - c)
    in
      if da <= db && da <= dc
        then
          left
        else
          if db <= dc
            then above
            else upperLeft
