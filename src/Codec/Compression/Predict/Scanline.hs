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
  , selectPredictedScanline
  , applyUnpredictorToScanline
  , fromPredictedLine
  ) where

import Codec.Compression.Flate qualified as FL
import Codec.Compression.Predict.Entropy
  ( Entropy (EntropyDeflate, EntropyLFS, EntropyMSAD, EntropyRLE, EntropyShannon, EntropySum)
  , entropyLFS
  , entropyMSAD
  , entropyShannon
  , entropySum
  )
import Codec.Compression.Predict.Predictor
  ( Predictor (PNGAverage, PNGNone, PNGOptimum, PNGPaeth, PNGSub, PNGUp, TIFFNoPrediction)
  , PredictorFunc
  , Samples (Samples)
  , decodeRowPredictor
  , getPredictorFunction
  , getUnpredictorFunction
  , isPNGGroup
  )
import Codec.Compression.RunLength qualified as RLE

import Data.Bitmap.BitmapConfiguration
  (BitmapConfiguration, bitmapPixelBytes, bitmapRawWidth)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Unsafe qualified as BSU
import Data.Fallible (Fallible)
import Data.Kind (Type)
import Data.List (maximumBy, minimumBy)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)

import Foreign.Storable (pokeByteOff)

import Util.ByteString (groupComponents, separateComponents)

{-|
A `Scanline` is a line of pixels.

Each scanline may have an associated `Predictor` indicating the state in which
the pixels are stored.
-}
type Scanline :: Type
data Scanline = Scanline
  { slPredictor :: !(Maybe Predictor) -- ^ Predictor used for this scanline
  , slStream    :: ![ByteString] -- ^ Scanline data separated by components
  }

{-|
An empty `Scanline` is used as a default `Scanline` when using PNG predictors
`PNGUp`, `PNGAverage` and `PNGPaeth`.

It’s just a serie of zero bytes.
-}
emptyScanline :: BitmapConfiguration -> Scanline
emptyScanline bitmapConfig = Scanline
  { slPredictor = Just TIFFNoPrediction
  , slStream    = separateComponents (bitmapPixelBytes bitmapConfig)
                   (BS.replicate (bitmapRawWidth bitmapConfig) 0)
  }

scanlineEntropy :: Entropy -> Scanline -> Double
scanlineEntropy EntropyShannon =
  entropyShannon . groupComponents . slStream

scanlineEntropy EntropyDeflate =
  FL.entropyCompress . groupComponents . slStream

scanlineEntropy EntropyRLE =
  RLE.entropyCompress . groupComponents . slStream

scanlineEntropy EntropySum =
  entropySum . groupComponents . slStream

scanlineEntropy EntropyLFS =
  entropyLFS . groupComponents . slStream

scanlineEntropy EntropyMSAD =
  entropyMSAD . groupComponents . slStream


{-|
Given a `Predictor` and 2 consecutive `Scanline`, encode the last `Scanline`.
-}
applyPredictorToScanline
  :: Entropy
  -> Predictor
  -> (Scanline, Scanline)
  -> Scanline
applyPredictorToScanline entropy PNGOptimum scanlines =
  selectPredictedScanline entropy
    [ applyPredictorToScanline entropy predictor scanlines
    | predictor <- [PNGNone, PNGSub, PNGUp, PNGAverage, PNGPaeth]
    ]

applyPredictorToScanline _ predictor (Scanline _ prior, Scanline _ current) =
  Scanline { slPredictor = Just predictor
           , slStream = zipWith encode prior current
           }
 where
  -- Preserve the previous zip semantics for incomplete rows.
  encode :: ByteString -> ByteString -> ByteString
  encode above currentBytes
    | predictor == PNGNone || predictor == TIFFNoPrediction
    = BS.take count currentBytes

    | otherwise
    = BSI.unsafeCreate count $ \dst ->
        let
          go :: Int -> Word8 -> Word8 -> IO ()
          go !offset !upperLeft !left
            | offset >= count = return ()
            | otherwise = do
                let
                  upper :: Word8
                  upper = BSU.unsafeIndex above offset

                  sample :: Word8
                  sample = BSU.unsafeIndex currentBytes offset

                pokeByteOff dst
                            offset
                            (fn (Samples upperLeft upper left sample))

                go (offset + 1) upper sample
        in
          go 0 0 0
   where
    count :: Int
    count = min (BS.length above) (BS.length currentBytes)

    fn :: PredictorFunc Word8
    fn = getPredictorFunction predictor

-- | Choose from shared candidates, preserving the original tie ordering.
selectPredictedScanline :: Entropy -> [Scanline] -> Scanline
selectPredictedScanline entropy candidates =
  let
    comparator
      :: ((Double, Scanline) -> (Double, Scanline) -> Ordering)
      -> [(Double, Scanline)]
      -> (Double, Scanline)
    comparator = if entropy == EntropyLFS
                  then maximumBy
                  else minimumBy

    scored :: [(Double, Scanline)]
    scored = [ (scanlineEntropy entropy candidate, candidate)
             | candidate <- candidates
             ]
  in
    snd $ comparator ((. fst) . compare . fst) scored

{-|
Given a `Predictor` and 2 consecutive `Scanline`, uncode the last `Scanline`.
-}
applyUnpredictorToScanline :: Predictor -> (Scanline, Scanline) -> Scanline
applyUnpredictorToScanline predictor ( Scanline _ prior
                                     , Scanline linePredictor current
                                     ) =
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
    :: PredictorFunc Word8
    -> (Word8, Word8)
    -> [(Word8, Word8)]
    -> [Word8]
  applyUnpredictorToScanline' _ _ [] = []
  applyUnpredictorToScanline' fn (upperLeft, left) ((above, sample) : remain) =
    let
      decodedSample :: Word8
      decodedSample = fn (Samples upperLeft above left sample)
    in
      decodedSample : applyUnpredictorToScanline' fn
                                                  (above, decodedSample)
                                                  remain

{-|
Convert a `ByteString` to a `Scanline` according to a `Predictor`.
-}
fromPredictedLine
  :: Predictor -> BitmapConfiguration -> ByteString -> Fallible Scanline
fromPredictedLine predictor bitmapConfig raw
  | isPNGGroup predictor
  = do
      let
        predictCode, bytes :: ByteString
        (predictCode, bytes) = BS.splitAt 1 raw

      linePredictor <- decodeRowPredictor (BS.head predictCode)

      return $ Scanline { slPredictor = Just linePredictor
                        , slStream = separateComponents
                                      (bitmapPixelBytes bitmapConfig)
                                      bytes
                        }
  | otherwise
  = return $ Scanline { slPredictor = Just predictor
                      , slStream = separateComponents
                                    (bitmapPixelBytes bitmapConfig)
                                    raw
                      }
