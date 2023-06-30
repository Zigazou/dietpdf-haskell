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
    ( PNGAverage
    , PNGNone
    , PNGOptimum
    , PNGPaeth
    , PNGSub
    , PNGUp
    , TIFFNoPrediction
    , TIFFPredictor2
    )
  , toPredictor
  , toRowPredictor
  , toWord8
  , toRowWord8
  , predict
  , unpredict
  , isPNGGroup
  , isTIFFGroup
  ) where

import qualified Data.ByteString               as BS
import           Data.Maybe                     ( fromMaybe )
import           Data.Word                      ( Word8 )
import           Util.ByteString                ( splitRaw
                                                , separateComponents
                                                , groupComponents
                                                )
import           Util.Errors                    ( UnifiedError
                                                  ( InvalidNumberOfBytes
                                                  , InvalidPredictor
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

{- | Convert a PDF predictor code, returns either a `Predictor` or an
`InvalidPredictor` error.
-}
toRowPredictor :: Word8 -> Either UnifiedError Predictor
toRowPredictor 0     = Right PNGNone
toRowPredictor 1     = Right PNGSub
toRowPredictor 2     = Right PNGUp
toRowPredictor 3     = Right PNGAverage
toRowPredictor 4     = Right PNGPaeth
toRowPredictor 5     = Right PNGOptimum
toRowPredictor value = Left $ InvalidPredictor value

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

{- | Convert a `Predictor` to a PDF row predictor code.
-}
toRowWord8 :: Predictor -> Word8
toRowWord8 PNGNone            = 0
toRowWord8 PNGSub             = 1
toRowWord8 PNGUp              = 2
toRowWord8 PNGAverage         = 3
toRowWord8 PNGPaeth           = 4
toRowWord8 PNGOptimum         = 5
toRowWord8 _anyOtherPredictor = 0

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

{- |
A `Scanline` is a line of pixels.

Each scanline may have an associated `Predictor` indicating the state in which
the pixels are stored.
-}
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

{-|
An image stream is a structure holding samples from an image stream while
allowing easier handling when applying predictors.
-}
data ImageStream = ImageStream
  { iWidth            :: Int
  , iComponents       :: Int
  , iBitsPerComponent :: Int
  , iPredictor        :: Maybe Predictor
  , iLines            :: [Scanline]
  }

{- |
A `Samples` is a utilitary structure used to facilitate computations of
predictors. It holds a sample and it’s 3 preceding samples.
-}
data Samples = Samples
  { sUpperLeft :: Word8
  , sAbove     :: Word8
  , sLeft      :: Word8
  , sCurrent   :: Word8
  }

{- |
A predictor function is a function taking samples as input and returning the
resulting sample.
-}
type PredictorFunc = Samples -> Word8

{- |
The `PNGAverage` predictor needs to do average on a larger scale than a simple
byte.
-}
average :: Word8 -> Word8 -> Word8
average a b =
  let a', b' :: Int
      (a', b') = (fromIntegral a, fromIntegral b)
  in  (fromIntegral . fst) (divMod (a' + b') 2)

{- |
The Paeth algorithm needs this estimating function.
-}
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

{- |
Returns the predictor function for a specified `Predictor`.

The function works on uncoded samples.
-}
predictF :: Predictor -> PredictorFunc
predictF PNGNone    = sCurrent
predictF PNGSub     = \s -> sCurrent s - sLeft s
predictF PNGUp      = \s -> sCurrent s - sAbove s
predictF PNGAverage = \s -> sCurrent s - average (sLeft s) (sAbove s)
predictF PNGPaeth =
  \s -> sCurrent s - paethBest (sLeft s) (sAbove s) (sUpperLeft s)
predictF _anyOtherPredictor = sCurrent

{- |
Returns the un-predictor function for a specified `Predictor`.

The function works on encoded samples.
-}
unpredictF :: Predictor -> PredictorFunc
unpredictF PNGNone    = sCurrent
unpredictF PNGSub     = \s -> sCurrent s + sLeft s
unpredictF PNGUp      = \s -> sCurrent s + sAbove s
unpredictF PNGAverage = \s -> sCurrent s + average (sLeft s) (sAbove s)
unpredictF PNGPaeth =
  \s -> sCurrent s + paethBest (sLeft s) (sAbove s) (sUpperLeft s)
unpredictF _anyOtherPredictor = sCurrent

{- |
Given a `Predictor` and 2 consecutive `Scanline`, encode the last `Scanline`.
-}
applyPredictor :: Predictor -> (Scanline, Scanline) -> Scanline
applyPredictor predictor (Scanline _ prior, Scanline _ current) = Scanline
  { slPredictor = Just predictor
  , slStream    = BS.pack
                    <$> (   applyPredictor' (predictF predictor) (0, 0)
                        .   uncurry BS.zip
                        <$> zip prior current
                        )
  }
 where
  applyPredictor'
    :: PredictorFunc -> (Word8, Word8) -> [(Word8, Word8)] -> [Word8]
  applyPredictor' _ _ [] = []
  applyPredictor' fn (upperLeft, left) ((above, sample) : remain) =
    fn (Samples upperLeft above left sample)
      : applyPredictor' fn (above, sample) remain

{- |
Encode an entire image stream using a specified `Predictor`
-}
predictImageStream :: Predictor -> ImageStream -> ImageStream
predictImageStream predictor imgStm = imgStm
  { iPredictor = Just predictor
  , iLines     = applyPredictor predictor <$> previousCurrent
                   (emptyScanline (iWidth imgStm) (iComponents imgStm))
                   (iLines imgStm)
  }
 where
  previousCurrent :: a -> [a] -> [(a, a)]
  previousCurrent previous currents = zip (previous : currents) currents

{- |
Given a `Predictor` and 2 consecutive `Scanline`, uncode the last `Scanline`.
-}
applyUnpredictor :: Predictor -> (Scanline, Scanline) -> Scanline
applyUnpredictor predictor (Scanline _ prior, Scanline linePredictor current) =
  Scanline
    { slPredictor = Nothing
    , slStream    =
      BS.pack
        <$> (   applyUnpredictor'
                (unpredictF (fromMaybe predictor linePredictor))
                (0, 0)
            .   uncurry BS.zip
            <$> zip prior current
            )
    }
 where
  applyUnpredictor'
    :: PredictorFunc -> (Word8, Word8) -> [(Word8, Word8)] -> [Word8]
  applyUnpredictor' _ _ [] = []
  applyUnpredictor' fn (upperLeft, left) ((above, sample) : remain) =
    let decodedSample = fn (Samples upperLeft above left sample)
    in  decodedSample : applyUnpredictor' fn (above, decodedSample) remain

{- |
Decode an entire image stream using a specified `Predictor`
-}
unpredictImageStream :: Predictor -> ImageStream -> ImageStream
unpredictImageStream predictor imgStm = imgStm
  { iPredictor = Nothing
  , iLines     = unpredictScanlines
                   (emptyScanline (iWidth imgStm) (iComponents imgStm))
                   (iLines imgStm)
  }
 where
  unpredictScanlines :: Scanline -> [Scanline] -> [Scanline]
  unpredictScanlines _ [] = []
  unpredictScanlines previous (current : remain) =
    let decodedLine = applyUnpredictor predictor (previous, current)
    in  decodedLine : unpredictScanlines decodedLine remain

{- |
Convert a `ByteString` to a `Scanline` according to a `Predictor`.
-}
fromPredictedLine
  :: Predictor -> Int -> BS.ByteString -> Either UnifiedError Scanline
fromPredictedLine predictor width raw
  | isPNGGroup predictor = do
    let (predictCode, bytes) = BS.splitAt 1 raw
    linePredictor <- toRowPredictor (BS.head predictCode)
    return $ Scanline { slPredictor = Just linePredictor
                      , slStream    = splitRaw width bytes
                      }
  | otherwise = return
  $ Scanline { slPredictor = Just predictor, slStream = splitRaw width raw }

{- |
Convert a `ByteString` to an `ImageStream` according to a `Predictor` and a
line width.
-}
fromPredictedStream
  :: Predictor -> Int -> Int -> BS.ByteString -> Either UnifiedError ImageStream
fromPredictedStream predictor width components raw = do
  let rawWidth = components * width + if isPNGGroup predictor then 1 else 0
  scanlines <- mapM (fromPredictedLine predictor width) (splitRaw rawWidth raw)
  return ImageStream { iWidth            = width
                     , iComponents       = components
                     , iBitsPerComponent = 8
                     , iPredictor        = Just TIFFNoPrediction
                     , iLines            = scanlines
                     }

{- |
Convert an `ImageStream` to a `ByteString`.
-}
packStream :: ImageStream -> BS.ByteString
packStream = BS.concat . fmap packScanline . iLines
 where
  packScanline :: Scanline -> BS.ByteString
  packScanline (Scanline Nothing stream) = groupComponents stream
  packScanline (Scanline (Just predictor) stream)
    | isPNGGroup predictor = BS.cons (toRowWord8 predictor)
                                     (groupComponents stream)
    | otherwise = groupComponents stream

{- |
Convert an unpredicted `Bytestring` to an `ImageStream` given its line width.
-}
fromUnpredictedStream
  :: Int -> Int -> BS.ByteString -> Either UnifiedError ImageStream
fromUnpredictedStream width components raw = return ImageStream
  { iWidth            = width
  , iComponents       = components
  , iBitsPerComponent = 8
  , iPredictor        = Nothing
  , iLines            = Scanline Nothing
                        .   separateComponents components
                        <$> splitRaw (width * components) raw
  }

{- |
Apply a `Predictor` to a `ByteString`, considering its line width.
-}
predict
  :: Predictor -- ^ Predictor to be used to encode
  -> Int -- ^ Width of the stream
  -> Int -- ^ Number of color components
  -> BS.ByteString -- ^ Stream to encode
  -> Either UnifiedError BS.ByteString -- ^ Encoded stream or an error
predict predictor width components stream
  | width < 1 = Left $ InvalidNumberOfBytes 0 0
  | otherwise = do
    imgStm <- fromUnpredictedStream width components stream
    let predicted = predictImageStream predictor imgStm
    return $ packStream predicted

{- |
Invert the application of a `Predictor` to a `ByteString`, considering its
line width.
-}
unpredict
  :: Predictor -- ^ Predictor (hint in case of a PNG predictor)
  -> Int -- ^ Width of the image
  -> Int -- ^ Number of color components
  -> BS.ByteString -- ^ Stream to decode
  -> Either UnifiedError BS.ByteString -- ^ Decoded stream or an error
unpredict predictor width components stream
  | width < 1 = Left $ InvalidNumberOfBytes 0 0
  | otherwise = do
    imgStm <- fromPredictedStream predictor width components stream
    let unpredicted = unpredictImageStream predictor imgStm
    return $ packStream unpredicted
