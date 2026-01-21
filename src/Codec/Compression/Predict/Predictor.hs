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
predictors is enabled globally for the image while PNG predictors can be changed
on every scanline.
-}
module Codec.Compression.Predict.Predictor
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
  , Samples(Samples, sAbove, sCurrent, sLeft, sUpperLeft)
  , decodePredictor
  , decodeRowPredictor
  , encodePredictor
  , encodeRowPredictor
  , isPNGGroup
  , isTIFFGroup
  , PredictorFunc
  , getPredictorFunction
  , getUnpredictorFunction
  ) where

import Data.Fallible (Fallible)
import Data.Kind (Type)
import Data.UnifiedError (UnifiedError (InvalidPredictor))
import Data.Word (Word8)

import PDF.Object.Object (PDFObject (PDFNumber), ToPDFNumber (mkPDFNumber))

type Predictor :: Type
data Predictor
  = TIFFNoPrediction
    -- ^ No prediction.
  | TIFFPredictor2
    -- ^ TIFF Predictor 2 predicts that each colour component of a sample is the
    --   same as the corresponding colour component of the sample immediately to
    --   its left.
  | PNGNone
    -- ^ With the None filter, the scanline is transmitted unmodified; it is
    --   only necessary to insert a filter type byte before the data.
  | PNGSub
    -- ^ The Sub filter transmits the difference between each byte and the value
    --   of the corresponding byte of the prior pixel.
  | PNGUp
    -- ^ The Up filter is just like the Sub filter except that the pixel
    --   immediately above the current pixel, rather than just to its left, is
    --   used as the predictor.
  | PNGAverage
    -- ^ The Average filter uses the average of the two neighboring pixels (left
    --   and above) to predict the value of a pixel.
  | PNGPaeth
    -- ^ The Paeth filter computes a simple linear function of the three
    --   neighboring pixels (left, above, upper left), then chooses as predictor
    --   the neighboring pixel closest to the computed value. This technique is
    --   due to Alan W. Paeth.
  | PNGOptimum
    -- ^ The optimum predictor is determined for each scanline.
  deriving stock (Eq, Read, Show)

{-| Convert a PDF predictor code, returns either a `Predictor` or an
`InvalidPredictor` error.
-}
decodePredictor :: Word8 -> Fallible Predictor
decodePredictor 1     = Right TIFFNoPrediction
decodePredictor 2     = Right TIFFPredictor2
decodePredictor 10    = Right PNGNone
decodePredictor 11    = Right PNGSub
decodePredictor 12    = Right PNGUp
decodePredictor 13    = Right PNGAverage
decodePredictor 14    = Right PNGPaeth
decodePredictor 15    = Right PNGOptimum
decodePredictor value = Left $ InvalidPredictor value

{-| Convert a PDF predictor code, returns either a `Predictor` or an
`InvalidPredictor` error.
-}
decodeRowPredictor :: Word8 -> Fallible Predictor
decodeRowPredictor 0     = Right PNGNone
decodeRowPredictor 1     = Right PNGSub
decodeRowPredictor 2     = Right PNGUp
decodeRowPredictor 3     = Right PNGAverage
decodeRowPredictor 4     = Right PNGPaeth
decodeRowPredictor 5     = Right PNGOptimum
decodeRowPredictor value = Left $ InvalidPredictor value

{-| Convert a `Predictor` to a PDF predictor code.
-}
encodePredictor :: Predictor -> Word8
encodePredictor TIFFNoPrediction = 1
encodePredictor TIFFPredictor2   = 2
encodePredictor PNGNone          = 10
encodePredictor PNGSub           = 11
encodePredictor PNGUp            = 12
encodePredictor PNGAverage       = 13
encodePredictor PNGPaeth         = 14
encodePredictor PNGOptimum       = 15

instance ToPDFNumber Predictor where
  mkPDFNumber :: Predictor -> PDFObject
  mkPDFNumber = PDFNumber . fromIntegral . encodePredictor

{-| Convert a `Predictor` to a PDF row predictor code.
-}
encodeRowPredictor :: Predictor -> Word8
encodeRowPredictor PNGNone            = 0
encodeRowPredictor PNGSub             = 1
encodeRowPredictor PNGUp              = 2
encodeRowPredictor PNGAverage         = 3
encodeRowPredictor PNGPaeth           = 4
encodeRowPredictor PNGOptimum         = 5
encodeRowPredictor _anyOtherPredictor = 0

{-|
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

{-|
Tell if a `Predictor` is from the TIFF group.

The TIFF group is applied globally, it adds no data to the stream.
-}
isTIFFGroup :: Predictor -> Bool
isTIFFGroup = not . isPNGGroup

{-|
A `Samples` is a utilitary structure used to facilitate computations of
predictors. It holds a sample and it’s 3 preceding samples.

The type parameter `a` represents the sample type, which can be:
- `Word8` for 8-bit samples (PNG and 8-bit TIFF)
- `Word16` for 16-bit samples (16-bit TIFF)
- Other integral types for 2-bit or 4-bit samples (unpacked from bytes)
-}
type role Samples representational
type Samples :: Type -> Type
data Samples a = Samples
  { sUpperLeft :: !a
  , sAbove     :: !a
  , sLeft      :: !a
  , sCurrent   :: !a
  }

{-|
A predictor function is a function taking samples as input and returning the
resulting sample.

The function is generic over the sample type to support different bit depths.
-}
type PredictorFunc :: Type -> Type
type PredictorFunc a = Samples a -> a

{-|
The `PNGAverage` predictor needs to do average on a larger scale than a simple
byte. Works with any integral type to support different bit depths.
-}
average :: Integral a => a -> a -> a
average a b =
  let a', b' :: Integer
      (a', b') = (fromIntegral a, fromIntegral b)
  in  (fromIntegral . fst) (divMod (a' + b') 2)

{-|
The Paeth algorithm needs this estimating function.
Works with any integral type to support different bit depths.
-}
paethBest :: Integral a => a -> a -> a -> a
paethBest left above upperLeft =
  let estimate :: Integer
      estimate =
        fromIntegral left + fromIntegral above - fromIntegral upperLeft

      distanceLeft, distanceAbove, distanceUpperLeft :: Integer
      distanceLeft      = abs (estimate - fromIntegral left)
      distanceAbove     = abs (estimate - fromIntegral above)
      distanceUpperLeft = abs (estimate - fromIntegral upperLeft)
  in  if distanceLeft <= distanceAbove && distanceLeft <= distanceUpperLeft
        then left
        else if distanceAbove <= distanceUpperLeft then above else upperLeft

{-|
Returns the predictor function for a specified `Predictor`.

The function works on uncoded samples and is generic over the sample type to
support different bit depths (8-bit, 16-bit, etc.).
-}
getPredictorFunction :: Integral a => Predictor -> Samples a -> a
getPredictorFunction PNGSub     s = sCurrent s - sLeft s
getPredictorFunction PNGUp      s = sCurrent s - sAbove s
getPredictorFunction PNGAverage s = sCurrent s - average (sLeft s) (sAbove s)
getPredictorFunction PNGPaeth s =
  sCurrent s - paethBest (sLeft s) (sAbove s) (sUpperLeft s)
getPredictorFunction TIFFPredictor2 s = sCurrent s - sLeft s
getPredictorFunction _anyOtherPredictor s = sCurrent s

{-|
Returns the un-predictor function for a specified `Predictor`.

The function works on encoded samples and is generic over the sample type to
support different bit depths (8-bit, 16-bit, etc.).
-}
getUnpredictorFunction :: Integral a => Predictor -> Samples a -> a
getUnpredictorFunction PNGSub     s = sCurrent s + sLeft s
getUnpredictorFunction PNGUp      s = sCurrent s + sAbove s
getUnpredictorFunction PNGAverage s = sCurrent s + average (sLeft s) (sAbove s)
getUnpredictorFunction PNGPaeth s =
  sCurrent s + paethBest (sLeft s) (sAbove s) (sUpperLeft s)
getUnpredictorFunction TIFFPredictor2 s = sCurrent s + sLeft s
getUnpredictorFunction _anyOtherPredictor s = sCurrent s
