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
module Codec.Compression.Predict.ImageStream
  ( packStream
  , fromPredictedStream
  , fromUnpredictedStream
  , unpredictImageStream
  , predictImageStream
  ) where

import Codec.Compression.Predict.Entropy (Entropy)
import Codec.Compression.Predict.Predictor
    ( Predictor (TIFFNoPrediction)
    , isPNGGroup
    , encodeRowPredictor
    )
import Codec.Compression.Predict.Scanline
    ( Scanline (Scanline)
    , applyPredictorToScanline
    , applyUnpredictorToScanline
    , emptyScanline
    , fromPredictedLine
    )

import Data.ByteString qualified as BS
import Data.Kind (Type)

import Util.ByteString (groupComponents, separateComponents, splitRaw)
import Util.UnifiedError (UnifiedError)

{-|
An image stream is a structure holding samples from an image stream while
allowing easier handling when applying predictors.
-}
type ImageStream :: Type
data ImageStream = ImageStream
  { iWidth            :: Int
  , iComponents       :: Int
  , iBitsPerComponent :: Int
  , iPredictor        :: Maybe Predictor
  , iLines            :: [Scanline]
  }

{- |
Encode an entire image stream using a specified `Predictor`
-}
predictImageStream :: Entropy -> Predictor -> ImageStream -> ImageStream
predictImageStream entropy predictor imgStm = imgStm
  { iPredictor = Just predictor
  , iLines     = applyPredictorToScanline entropy predictor <$> previousCurrent
                   (emptyScanline (iWidth imgStm) (iComponents imgStm))
                   (iLines imgStm)
  }
 where
  previousCurrent :: a -> [a] -> [(a, a)]
  previousCurrent previous currents = zip (previous : currents) currents

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
    let decodedLine = applyUnpredictorToScanline predictor (previous, current)
    in  decodedLine : unpredictScanlines decodedLine remain

{- |
Convert a `ByteString` to an `ImageStream` according to a `Predictor` and a
line width.
-}
fromPredictedStream
  :: Predictor -> Int -> Int -> BS.ByteString -> Either UnifiedError ImageStream
fromPredictedStream predictor width components raw = do
  let rawWidth = components * width + if isPNGGroup predictor then 1 else 0
  scanlines <- mapM (fromPredictedLine predictor components) (splitRaw rawWidth raw)
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
    | isPNGGroup predictor = BS.cons (encodeRowPredictor predictor)
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

