-- | Read-only analysis of decoded alpha samples. No quantization is performed.
module Data.Bitmap.MaskAnalysis
  ( MaskClass (..), MaskThresholds (..), defaultMaskThresholds
  , MaskAnalysis (..), PixelBounds (..), analyzeMask
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Kind (Type)

-- | Bounds use inclusive, zero-based pixel coordinates. Nontrivial means
-- strictly between transparent and opaque, rather than merely nonzero.
type PixelBounds :: Type
data PixelBounds = PixelBounds !Int !Int !Int !Int deriving stock (Eq, Show)

type MaskClass :: Type
data MaskClass
  = BinaryMask
  | NearlyBinaryMask
  | SmoothMask
  | DetailedMask
  deriving stock (Eq, Show)

-- | Fractions lie in [0,1]; the neighbor difference is in alpha units [1,255].
-- Defaults are heuristics, not guarantees about visual quality.
type MaskThresholds :: Type
data MaskThresholds = MaskThresholds
  { nearlyBinaryFraction :: !Double
  , largeDifference      :: !Int
  , detailedFraction     :: !Double
  } deriving stock (Eq, Show)

defaultMaskThresholds :: MaskThresholds
defaultMaskThresholds = MaskThresholds 0.01 32 0.05

type MaskAnalysis :: Type
data MaskAnalysis = MaskAnalysis
  { maskClass               :: !MaskClass
  , histogram               :: !(IntMap Int)
  , distinctValues          :: !Int
  , pixelCount              :: !Int
  , transparentFraction     :: !Double
  , opaqueFraction          :: !Double
  , intermediateFraction    :: !Double
  , intermediateBounds      :: !(Maybe PixelBounds)
  , largeDifferenceFraction :: !Double
  } deriving stock (Eq, Show)

-- | Analyze one 8-bit alpha per pixel. Reject invalid geometry, length and
-- thresholds. Compare horizontal and vertical neighbors, never across rows.
analyzeMask
  :: MaskThresholds
  -> Int
  -> Int
  -> ByteString
  -> Either String MaskAnalysis
analyzeMask thresholds width height samples
  | width <= 0 || height <= 0
  = Left "Invalid mask dimensions"

  | toInteger width * toInteger height /= toInteger (BS.length samples)
  = Left "Invalid mask sample count"

  | not (validFraction (nearlyBinaryFraction thresholds))
    || not (validFraction (detailedFraction thresholds))
    || largeDifference thresholds < 1
    || largeDifference thresholds > 255
  = Left "Invalid mask thresholds"

  | otherwise
  = Right (analyzeValidMask thresholds width samples)
 where
  validFraction :: Double -> Bool
  validFraction x = x >= 0 && x <= 1

analyzeValidMask :: MaskThresholds -> Int -> ByteString -> MaskAnalysis
analyzeValidMask thresholds width samples =
  MaskAnalysis
    { maskClass = classification
    , histogram = counts
    , distinctValues = IM.size counts
    , pixelCount = total
    , transparentFraction = fraction zeros total
    , opaqueFraction = fraction ones total
    , intermediateFraction = middleFraction
    , intermediateBounds = bounds
    , largeDifferenceFraction = edgeFraction
    }
 where
  total :: Int
  total = BS.length samples

  counts :: IntMap Int
  counts = BS.foldl'
            (\acc value -> IM.insertWith (+) (fromIntegral value) 1 acc)
            IM.empty
            samples

  zeros :: Int
  zeros = IM.findWithDefault 0 0 counts

  ones :: Int
  ones = IM.findWithDefault 0 255 counts

  middleFraction :: Double
  middleFraction = fraction (total - zeros - ones) total

  fraction :: Int -> Int -> Double
  fraction a b = if b == 0 then 0 else fromIntegral a / fromIntegral b

  bounds :: Maybe PixelBounds
  edges :: Int
  pairs :: Int
  (bounds, edges, pairs) = scan 0 Nothing (0 :: Int) (0 :: Int)

  -- The scan retains only a histogram and constant-sized geometry statistics.
  scan
    :: Int
    -> Maybe PixelBounds
    -> Int
    -> Int
    -> (Maybe PixelBounds, Int, Int)
  scan i box large neighbors
    | i == total
    = (box, large, neighbors)

    | otherwise
    = let
        value :: Int
        value = fromIntegral (BS.index samples i) :: Int

        x :: Int
        x = i `mod` width

        y :: Int
        y = i `div` width

        box' :: Maybe PixelBounds
        box' = if value == 0 || value == 255
                then
                  box
                else
                  Just $ case box of
                    Nothing -> PixelBounds x y x y
                    Just (PixelBounds x0 y0 x1 y1) -> PixelBounds (min x0 x)
                                                                  (min y0 y)
                                                                  (max x1 x)
                                                                  (max y1 y)
  
        isLarge :: Int -> Int
        isLarge offset =
          fromEnum (abs ( value
                        - fromIntegral (BS.index samples (i - offset))
                        ) >= largeDifference thresholds
                   )

        horizontal :: Int
        horizontal = if x > 0 then isLarge 1 else 0

        vertical :: Int
        vertical = if y > 0 then isLarge width else 0
      in
        scan (i + 1)
             box'
             (large + horizontal + vertical)
             (neighbors + fromEnum (x > 0) + fromEnum (y > 0))

  edgeFraction :: Double
  edgeFraction = fraction edges pairs

  classification :: MaskClass
  classification
    | zeros + ones == total
    = BinaryMask

    | middleFraction < nearlyBinaryFraction thresholds
    = NearlyBinaryMask

    | edges > 0 && edgeFraction >= detailedFraction thresholds
    = DetailedMask
  
    | otherwise
    = SmoothMask
