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
  , predictPNGImageStreams
  ) where

import Codec.Compression.Predict.Entropy (Entropy (EntropyShannon))
import Codec.Compression.Predict.Predictor
  ( Predictor (PNGAverage, PNGNone, PNGOptimum, PNGPaeth, PNGSub, PNGUp)
  , encodeRowPredictor
  , isPNGGroup
  )
import Codec.Compression.Predict.Scanline
  ( Scanline (Scanline)
  , applyPredictorToScanline
  , applyUnpredictorToScanline
  , emptyScanline
  , fromPredictedLine
  , selectPredictedScanline
  )

import Data.Bitmap.BitmapConfiguration
  (BitmapConfiguration, bitmapPixelBytes, bitmapRawWidth)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Fallible (Fallible)
import Data.Kind (Type)
import Data.List (transpose)

import Util.ByteString (groupComponents, separateComponents, splitRaw)

{-|
An image stream is a structure holding samples from an image stream while
allowing easier handling when applying predictors.
-}
type ImageStream :: Type
data ImageStream = ImageStream
  { iBitmapConfig :: !BitmapConfiguration -- ^ Bitmap configuration
  , iPredictor    :: !(Maybe Predictor) -- ^ Applied predictor, if any
  , iLines        :: ![Scanline] -- ^ Image scanlines
  }

{-|
Encode an entire image stream using a specified `Predictor`
-}
predictImageStream :: Entropy -> Predictor -> ImageStream -> ImageStream
predictImageStream entropy predictor imgStm = imgStm
  { iPredictor = Just predictor
  , iLines     = applyPredictorToScanline entropy predictor <$> previousCurrent
                   (emptyScanline (iBitmapConfig imgStm))
                   (iLines imgStm)
  }
 where
  previousCurrent :: a -> [a] -> [(a, a)]
  previousCurrent previous currents = zip (previous : currents) currents

-- | Evaluate all requested PNG strategies against the same five candidates
-- per row. Force row selections together so unselected buffers can be released.
predictPNGImageStreams :: [(Entropy, Predictor)] -> ImageStream -> [ByteString]
predictPNGImageStreams requests imgStm
  | null requests = []
  | null (iLines imgStm) = replicate (length requests) BS.empty
  | otherwise = map BS.concat $ transpose $ map selectRow pairs
 where
  pairs :: [(Scanline, Scanline)]
  pairs = zip (emptyScanline (iBitmapConfig imgStm) : iLines imgStm)
              (iLines imgStm)

  predictors :: [Predictor]
  predictors = [PNGNone, PNGSub, PNGUp, PNGAverage, PNGPaeth]

  selectRow :: (Scanline, Scanline) -> [ByteString]
  selectRow pair =
    let
      candidates :: [Scanline]
      candidates = [applyPredictorToScanline EntropyShannon p pair
                   | p <- predictors
                   ]

      choose :: (Entropy, Predictor) -> Scanline
      choose (criterion, PNGOptimum) = selectPredictedScanline criterion
                                                               candidates
      choose (criterion, predictor) =
          case lookup predictor (zip predictors candidates) of
            Just candidate -> candidate
            Nothing        -> applyPredictorToScanline criterion predictor pair

      packed :: [ByteString]
      packed = [ packScanline (choose request)
               | request <- requests
               ]
    in
      foldr seq packed packed

{-|
Decode an entire image stream using a specified `Predictor`
-}
unpredictImageStream :: Predictor -> ImageStream -> ImageStream
unpredictImageStream predictor imgStm = imgStm
  { iPredictor = Nothing
  , iLines     = unpredictScanlines
                   (emptyScanline (iBitmapConfig imgStm))
                   (iLines imgStm)
  }
 where
  unpredictScanlines :: Scanline -> [Scanline] -> [Scanline]
  unpredictScanlines _ [] = []
  unpredictScanlines previous (current : remain) =
    let decodedLine = applyUnpredictorToScanline predictor (previous, current)
    in  decodedLine : unpredictScanlines decodedLine remain

{-|
Convert a `ByteString` to an `ImageStream` according to a `Predictor` and a
line width.
-}
fromPredictedStream
  :: Predictor -> BitmapConfiguration -> ByteString -> Fallible ImageStream
fromPredictedStream predictor bitmapConfig raw = do
  let rawWidth = bitmapRawWidth bitmapConfig
               + if isPNGGroup predictor then 1 else 0

  scanlines <- mapM (fromPredictedLine predictor bitmapConfig)
                    (splitRaw rawWidth raw)

  return ImageStream { iBitmapConfig = bitmapConfig
                     , iPredictor    = Just predictor
                     , iLines        = scanlines
                     }

{-|
Convert an `ImageStream` to a `ByteString`.
-}
packStream :: ImageStream -> ByteString
packStream = BS.concat . fmap packScanline . iLines

packScanline :: Scanline -> ByteString
packScanline (Scanline Nothing stream) = groupComponents stream
packScanline (Scanline (Just predictor) stream)
  | BS.length rawLine == 0 = ""
  | isPNGGroup predictor = BS.cons (encodeRowPredictor predictor) rawLine
  | otherwise = rawLine
  where rawLine = groupComponents stream

{-|
Convert an unpredicted `Bytestring` to an `ImageStream` given its line width.
-}
fromUnpredictedStream
  :: BitmapConfiguration
  -> ByteString
  -> Fallible ImageStream
fromUnpredictedStream bitmapConfig raw = return ImageStream
  { iBitmapConfig     = bitmapConfig
  , iPredictor        = Nothing
  , iLines            = Scanline Nothing
                      . separateComponents (bitmapPixelBytes bitmapConfig)
                    <$> splitRaw (bitmapRawWidth bitmapConfig) raw
  }
