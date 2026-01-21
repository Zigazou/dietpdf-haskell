module Codec.Compression.Predict
  ( predict
  , unpredict
  , Entropy (EntropyDeflate, EntropyRLE, EntropyShannon)
  , Predictor (PNGOptimum, PNGAverage, PNGNone, PNGPaeth, PNGSub, PNGUp, TIFFNoPrediction, TIFFPredictor2)
  )
where

import Codec.Compression.Predict.Entropy
  (Entropy (EntropyDeflate, EntropyRLE, EntropyShannon))
import Codec.Compression.Predict.ImageStream
  ( fromPredictedStream
  , fromUnpredictedStream
  , packStream
  , predictImageStream
  , unpredictImageStream
  )
import Codec.Compression.Predict.Predictor
  ( Predictor (PNGAverage, PNGNone, PNGOptimum, PNGPaeth, PNGSub, PNGUp, TIFFNoPrediction, TIFFPredictor2)
  )
import Codec.Compression.Predict.TIFF (tiffPredictRow, tiffUnpredictRow)

import Data.Bitmap.BitmapConfiguration
  (BitmapConfiguration (bcLineWidth), bitmapRawWidth)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Fallible (Fallible)
import Data.UnifiedError (UnifiedError (InvalidNumberOfBytes))

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
  | predictor == TIFFPredictor2 =
     let rows = splitRaw (bitmapRawWidth bitmapConfig) stream
         predictedRows = tiffPredictRow bitmapConfig <$> rows
     in  return $ BS.concat predictedRows
  | otherwise = do
    imgStm <- fromUnpredictedStream bitmapConfig stream
    return $ packStream (predictImageStream entropy predictor imgStm)

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
  | bcLineWidth bitmapConfig < 1 = Left $ InvalidNumberOfBytes 0 0
  | predictor == TIFFPredictor2 =
     let rows = splitRaw (bitmapRawWidth bitmapConfig) stream
         unpredictedRows = tiffUnpredictRow bitmapConfig <$> rows
     in  return $ BS.concat unpredictedRows
  | otherwise = do
    imgStm <- fromPredictedStream predictor bitmapConfig stream
    let unpredicted = unpredictImageStream predictor imgStm
    return $ packStream unpredicted
