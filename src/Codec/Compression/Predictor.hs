{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}
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
module Codec.Compression.Predictor
  ( Predictor
    ( TIFFNoPrediction
    , TIFFPredictor2
    , PNGNone
    , PNGSub
    , PNGUp
    , PNGAverage
    , PNGPaeth
    , PNGOptimum
    )
  , toPredictor
  , toWord8
  , predict
  , unpredict
  , isPNGGroup
  , isTIFFGroup
  ) where

import qualified Data.ByteString               as BS
import           Data.Word                      ( Word8 )
import           Util.ByteString                ( splitRaw )
import           Util.Errors                    ( UnifiedError
                                                  ( InvalidPredictor
                                                  , InvalidNumberOfBytes
                                                  )
                                                )

data Predictor
  = TIFFNoPrediction
    -- ^ No prediction.
  | TIFFPredictor2
    -- ^ TIFF Predictor 2 predicts that each colour component of a sample is
    --   the same as the corresponding colour component of the sample
    --   immediately to its left.
  | PNGNone
    -- ^ With the None filter, the scanline is transmitted unmodified; it
    --   is only necessary to insert a filter type byte before the data.
  | PNGSub
    -- ^ The Sub filter transmits the difference between each byte and the
    --   value of the corresponding byte of the prior pixel.  
  | PNGUp
    -- ^ The Up filter is just like the Sub filter except that the pixel
    --   immediately above the current pixel, rather than just to its left,
    --   is used as the predictor.
  | PNGAverage
    -- ^ The Average filter uses the average of the two neighboring pixels
    --   (left and above) to predict the value of a pixel.
  | PNGPaeth
    -- ^ The Paeth filter computes a simple linear function of the three
    --   neighboring pixels (left, above, upper left), then chooses as
    --   predictor the neighboring pixel closest to the computed value.
    --   This technique is due to Alan W. Paeth.
  | PNGOptimum
    -- ^ The optimum predictor is determined for each scanline.
  deriving stock (Eq, Show)

{- | Convert a PDF predictor code, returns either a `Predictor` or an
`InvalidPredictor` error.
-}
toPredictor :: Word8 -> Either UnifiedError Predictor
toPredictor 1     = Right TIFFNoPrediction
toPredictor 2     = Right TIFFPredictor2
toPredictor 10    = Right PNGNone
toPredictor 11    = Right PNGSub
toPredictor 12    = Right PNGUp
toPredictor 13    = Right PNGAverage
toPredictor 14    = Right PNGPaeth
toPredictor 15    = Right PNGOptimum
toPredictor value = Left $ InvalidPredictor value

{- | Convert a `Predictor` to a PDF predictor code.
-}
toWord8 :: Predictor -> Word8
toWord8 TIFFNoPrediction = 1
toWord8 TIFFPredictor2   = 2
toWord8 PNGNone          = 10
toWord8 PNGSub           = 11
toWord8 PNGUp            = 12
toWord8 PNGAverage       = 13
toWord8 PNGPaeth         = 14
toWord8 PNGOptimum       = 15

{- |
Tell if a `Predictor` is from the PNG group.

Predictors from the PNG group work on a per-line basis. They add one byte to
each line to specify the predictor used on the line.
-}
isPNGGroup :: Predictor -> Bool
isPNGGroup TIFFNoPrediction = False
isPNGGroup TIFFPredictor2   = False
isPNGGroup PNGNone          = True
isPNGGroup PNGSub           = True
isPNGGroup PNGUp            = True
isPNGGroup PNGAverage       = True
isPNGGroup PNGPaeth         = True
isPNGGroup PNGOptimum       = True

{- |
Tell if a `Predictor` is from the TIFF group.

The TIFF group is applied globally, it adds no data to the stream.
-}
isTIFFGroup :: Predictor -> Bool
isTIFFGroup = not . isPNGGroup

data Scanline = Scanline
  { slPredictor :: Maybe Predictor
  , slStream    :: BS.ByteString
  }

emptyScanline :: Int -> Scanline
emptyScanline width = Scanline { slPredictor = Just TIFFNoPrediction
                               , slStream    = BS.pack $ replicate width 0
                               }

data ImageStream = ImageStream
  { iWidth            :: Int
  , iBitsPerComponent :: Int
  , iPredictor        :: Maybe Predictor
  , iLines            :: [Scanline]
  }

data Samples = Samples
  { sUpperLeft :: Word8
  , sAbove     :: Word8
  , sLeft      :: Word8
  , sCurrent   :: Word8
  }

type PredictorFunc = Samples -> Word8

average :: Word8 -> Word8 -> Word8
average a b =
  let a', b' :: Int
      (a', b') = (fromIntegral a, fromIntegral b)
  in  (fromIntegral . snd) (divMod (a' + b') 2)

paethBest :: Word8 -> Word8 -> Word8 -> Word8
paethBest left above upperLeft =
  let estimate :: Int
      estimate =
        fromIntegral left + fromIntegral above - fromIntegral upperLeft

      distanceLeft, distanceAbove, distanceUpperLeft :: Int
      distanceLeft      = abs (estimate - fromIntegral left)
      distanceAbove     = abs (estimate - fromIntegral above)
      distanceUpperLeft = abs (estimate - fromIntegral upperLeft)
  in  if distanceLeft <= distanceAbove && distanceLeft <= distanceUpperLeft
        then left
        else if distanceAbove <= distanceUpperLeft then above else upperLeft

predictF :: Predictor -> PredictorFunc
predictF PNGNone    = sCurrent
predictF PNGSub     = \s -> sCurrent s - sLeft s
predictF PNGUp      = \s -> sCurrent s - sAbove s
predictF PNGAverage = \s -> sCurrent s - average (sLeft s) (sAbove s)
predictF PNGPaeth =
  \s -> sCurrent s - paethBest (sLeft s) (sAbove s) (sUpperLeft s)
predictF _anyOtherPredictor = sCurrent

unpredictF :: Predictor -> PredictorFunc
unpredictF PNGNone    = sCurrent
unpredictF PNGSub     = \s -> sCurrent s + sLeft s
unpredictF PNGUp      = \s -> sCurrent s + sAbove s
unpredictF PNGAverage = \s -> sCurrent s + average (sLeft s) (sAbove s)
unpredictF PNGPaeth =
  \s -> sCurrent s + paethBest (sLeft s) (sAbove s) (sUpperLeft s)
unpredictF _anyOtherPredictor = sCurrent

applyPredictor :: Predictor -> (Scanline, Scanline) -> Scanline
applyPredictor predictor (Scanline _ prior, Scanline _ current) = Scanline
  { slPredictor = Just predictor
  , slStream    = BS.pack
    (applyPredictor' (predictF predictor) (0, 0) (BS.zip prior current))
  }
 where
  applyPredictor'
    :: PredictorFunc -> (Word8, Word8) -> [(Word8, Word8)] -> [Word8]
  applyPredictor' _ _ [] = []
  applyPredictor' fn (upperLeft, left) ((above, sample) : remain) =
    fn (Samples upperLeft above left sample)
      : applyPredictor' fn (above, sample) remain

predictImageStream :: Predictor -> ImageStream -> ImageStream
predictImageStream predictor imgStm = imgStm
  { iPredictor = Just predictor
  , iLines     = applyPredictor predictor <$> previousCurrent
                   (emptyScanline (iWidth imgStm))
                   (iLines imgStm)
  }
 where
  previousCurrent :: a -> [a] -> [(a, a)]
  previousCurrent previous currents = zip (previous : currents) currents

applyUnpredictor :: Predictor -> (Scanline, Scanline) -> Scanline
applyUnpredictor predictor (Scanline _ prior, Scanline _ current) = Scanline
  { slPredictor = Nothing
  , slStream    = BS.pack
    (applyUnpredictor' (unpredictF predictor) (0, 0) (BS.zip prior current))
  }
 where
  applyUnpredictor'
    :: PredictorFunc -> (Word8, Word8) -> [(Word8, Word8)] -> [Word8]
  applyUnpredictor' _ _ [] = []
  applyUnpredictor' fn (upperLeft, left) ((above, sample) : remain) =
    let decodedSample = fn (Samples upperLeft above left sample)
    in  decodedSample : applyUnpredictor' fn (above, decodedSample) remain

unpredictImageStream :: Predictor -> ImageStream -> ImageStream
unpredictImageStream predictor imgStm = imgStm
  { iPredictor = Nothing
  , iLines = unpredictScanlines (emptyScanline (iWidth imgStm)) (iLines imgStm)
  }
 where
  unpredictScanlines :: Scanline -> [Scanline] -> [Scanline]
  unpredictScanlines _ [] = []
  unpredictScanlines previous (current : remain) =
    let decodedLine = applyUnpredictor predictor (previous, current)
    in  decodedLine : unpredictScanlines decodedLine remain

fromPredictedLine :: Predictor -> BS.ByteString -> Either UnifiedError Scanline
fromPredictedLine predictor raw
  | isPNGGroup predictor = do
    let (predictCode, bytes) = BS.splitAt 1 raw
    linePredictor <- toPredictor (BS.head predictCode)
    return $ Scanline { slPredictor = Just linePredictor, slStream = bytes }
  | otherwise = return
  $ Scanline { slPredictor = Just predictor, slStream = raw }

fromPredictedStream
  :: Predictor -> Int -> BS.ByteString -> Either UnifiedError ImageStream
fromPredictedStream predictor width raw = do
  let rawWidth = width + if isPNGGroup predictor then 1 else 0
  scanlines <- mapM (fromPredictedLine predictor) (splitRaw rawWidth raw)
  return ImageStream { iWidth            = width
                     , iBitsPerComponent = 8
                     , iPredictor        = Just TIFFNoPrediction
                     , iLines            = scanlines
                     }

packStream :: ImageStream -> BS.ByteString
packStream = BS.concat . fmap packScanline . iLines
 where
  packScanline :: Scanline -> BS.ByteString
  packScanline (Scanline Nothing stream) = stream
  packScanline (Scanline (Just predictor) stream)
    | isPNGGroup predictor = BS.cons (toWord8 predictor) stream
    | otherwise            = stream

fromUnpredictedStream
  :: Int -> BS.ByteString -> Either UnifiedError ImageStream
fromUnpredictedStream width raw = return ImageStream
  { iWidth            = width
  , iBitsPerComponent = 8
  , iPredictor        = Nothing
  , iLines            = Scanline Nothing <$> splitRaw width raw
  }

{- |
Apply a `Predictor` to a `ByteString`, considering its line width.
-}
predict
  :: Predictor -> Int -> BS.ByteString -> Either UnifiedError BS.ByteString
predict predictor width stream
  | width < 1 = Left $ InvalidNumberOfBytes 0 0
  | otherwise = do
    imgStm <- fromUnpredictedStream width stream
    let predicted = predictImageStream predictor imgStm
    return $ packStream predicted

{- |
Invert the application of a `Predictor` to a `ByteString`, considering its
line width.
-}
unpredict
  :: Predictor -> Int -> BS.ByteString -> Either UnifiedError BS.ByteString
unpredict predictor width stream
  | width < 1 = Left $ InvalidNumberOfBytes 0 0
  | otherwise = do
    imgStm <- fromPredictedStream predictor width stream
    let unpredicted = unpredictImageStream predictor imgStm
    return $ packStream unpredicted
