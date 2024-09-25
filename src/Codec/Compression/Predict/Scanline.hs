{-|
This module implements the predictors as specified by the PDF reference.

There are 2 groups:

- TIFF predictors
- PNG predictors

TIFF predictors group only supports type 2 from the TIFF 6.0 specification
(https://www.itu.int/itudoc/itu-t/com16/tiff-fx/docs/tiff6.pdf, page 64).

PNG predictors group supports predictors defined in the RFC 2083
(https://www.rfc-editor.org/rfc/rfc2083.html).

Main difference between TIFF predictors and PNG predictors is that TIFF
predictors is enabled globally for the image while PNG predictors can be
changed on every scanline.
-}
module Codec.Compression.Predict.Scanline
  ( Scanline (Scanline, slPredictor, slStream)
  , emptyScanline
  , scanlineEntropy
  , applyPredictorToScanline
  , applyUnpredictorToScanline
  , fromPredictedLine
  ) where

import Codec.Compression.Flate qualified as FL
import Codec.Compression.Predict.Entropy
    ( Entropy (EntropyDeflate, EntropyRLE, EntropyShannon)
    , entropyShannon
    )
import Codec.Compression.Predict.Predictor
    ( Predictor (PNGAverage, PNGNone, PNGOptimum, PNGPaeth, PNGSub, PNGUp, TIFFNoPrediction)
    , PredictorFunc
    , Samples (Samples)
    , isPNGGroup
    , getPredictorFunction
    , decodeRowPredictor
    , getUnpredictorFunction
    )
import Codec.Compression.RunLength qualified as RLE

import Data.ByteString qualified as BS
import Data.Kind (Type)
import Data.List (minimumBy)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)

import Util.ByteString (groupComponents, separateComponents)
import Util.UnifiedError (UnifiedError)

{- |
A `Scanline` is a line of pixels.

Each scanline may have an associated `Predictor` indicating the state in which
the pixels are stored.
-}
type Scanline :: Type
data Scanline = Scanline
  { slPredictor :: Maybe Predictor
  , slStream    :: [BS.ByteString]
  }

{- |
An empty `Scanline` is used as a default `Scanline` when using PNG predictors
`PNGUp`, `PNGAverage` and `PNGPaeth`.

It’s just a serie of zero bytes.
-}
emptyScanline :: Int -> Int -> Scanline
emptyScanline width components = Scanline
  { slPredictor = Just TIFFNoPrediction
  , slStream    = replicate components (BS.pack $ replicate width 0)
  }

scanlineEntropy :: Entropy -> Scanline -> Double
scanlineEntropy EntropyShannon = entropyShannon . groupComponents . slStream
scanlineEntropy EntropyDeflate =
  FL.entropyCompress . groupComponents . slStream
scanlineEntropy EntropyRLE = RLE.entropyCompress . groupComponents . slStream

{- |
Given a `Predictor` and 2 consecutive `Scanline`, encode the last `Scanline`.
-}
applyPredictorToScanline :: Entropy -> Predictor -> (Scanline, Scanline) -> Scanline
applyPredictorToScanline entropy PNGOptimum scanlines =
  let allPredicted =
        applyPredictorToScanline
          <$> [entropy]
          <*> [PNGNone, PNGSub, PNGUp, PNGAverage, PNGPaeth]
          <*> [scanlines]
      entropies = ((,) =<< scanlineEntropy entropy) <$> allPredicted
  in  snd $ minimumBy ((. fst) . compare . fst) entropies

applyPredictorToScanline _ predictor (Scanline _ prior, Scanline _ current) = Scanline
  { slPredictor = Just predictor
  , slStream    = BS.pack
                . applyPredictorToScanline' (getPredictorFunction predictor) (0, 0) . uncurry BS.zip
                <$> zip prior current
  }
 where
  applyPredictorToScanline'
    :: PredictorFunc -> (Word8, Word8) -> [(Word8, Word8)] -> [Word8]
  applyPredictorToScanline' _ _ [] = []
  applyPredictorToScanline' fn (upperLeft, left) ((above, sample) : remain) =
    fn (Samples upperLeft above left sample)
      : applyPredictorToScanline' fn (above, sample) remain

{- |
Given a `Predictor` and 2 consecutive `Scanline`, uncode the last `Scanline`.
-}
applyUnpredictorToScanline :: Predictor -> (Scanline, Scanline) -> Scanline
applyUnpredictorToScanline predictor (Scanline _ prior, Scanline linePredictor current) =
  Scanline
    { slPredictor = Nothing
    , slStream    =
        BS.pack
          . applyUnpredictorToScanline'
              (getUnpredictorFunction (fromMaybe predictor linePredictor)) (0, 0)
              . uncurry BS.zip
          <$> zip prior current
    }
 where
  applyUnpredictorToScanline'
    :: PredictorFunc -> (Word8, Word8) -> [(Word8, Word8)] -> [Word8]
  applyUnpredictorToScanline' _ _ [] = []
  applyUnpredictorToScanline' fn (upperLeft, left) ((above, sample) : remain) =
    let decodedSample = fn (Samples upperLeft above left sample)
    in  decodedSample : applyUnpredictorToScanline' fn (above, decodedSample) remain

{- |
Convert a `ByteString` to a `Scanline` according to a `Predictor`.
-}
fromPredictedLine
  :: Predictor -> Int -> BS.ByteString -> Either UnifiedError Scanline
fromPredictedLine predictor components raw
  | isPNGGroup predictor = do
    let (predictCode, bytes) = BS.splitAt 1 raw
    linePredictor <- decodeRowPredictor (BS.head predictCode)
    return $ Scanline { slPredictor = Just linePredictor
                      , slStream    = separateComponents components bytes
                      }
  | otherwise =
    return $ Scanline { slPredictor = Just predictor
                      , slStream = separateComponents components raw
                      }
