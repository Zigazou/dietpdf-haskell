module Codec.Compression.Predict
  ( predict
  , unpredict
  , Entropy (EntropyDeflate, EntropyRLE, EntropyShannon)
  , Predictor (PNGOptimum, PNGAverage, PNGNone, PNGPaeth, PNGSub, PNGUp, TIFFNoPrediction, TIFFPredictor2)
  )
where

import Codec.Compression.Predict.Entropy
    ( Entropy (EntropyDeflate, EntropyRLE, EntropyShannon)
    )
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

import Data.ByteString (ByteString)
import Data.Fallible (Fallible)
import Data.UnifiedError (UnifiedError (InvalidNumberOfBytes))

{-|
Apply a `Predictor` to a `ByteString`, considering its line width.
-}
predict
  :: Entropy -- ^ Entropy type to use
  -> Predictor -- ^ Predictor to be used to encode
  -> Int -- ^ Width of the stream
  -> Int -- ^ Number of color components
  -> ByteString -- ^ Stream to encode
  -> Fallible ByteString -- ^ Encoded stream or an error
predict entropy predictor width components stream
  | width < 1 = Left $ InvalidNumberOfBytes 0 0
  | otherwise = do
    imgStm <- fromUnpredictedStream width components stream
    return $ packStream (predictImageStream entropy predictor imgStm)

{-|
Invert the application of a `Predictor` to a `ByteString`, considering its
line width.
-}
unpredict
  :: Predictor -- ^ Predictor (hint in case of a PNG predictor)
  -> Int -- ^ Width of the image
  -> Int -- ^ Number of color components
  -> ByteString -- ^ Stream to decode
  -> Fallible ByteString -- ^ Decoded stream or an error
unpredict predictor width components stream
  | width < 1 = Left $ InvalidNumberOfBytes 0 0
  | otherwise = do
    imgStm <- fromPredictedStream predictor width components stream
    let unpredicted = unpredictImageStream predictor imgStm
    return $ packStream unpredicted
