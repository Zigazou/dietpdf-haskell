{-|
Predictor + Deflate filter combination.

Finds an optimal PNG predictor using both Shannon and Deflate-oriented
heuristics, compresses with fast Deflate, and returns a `FilterCombination` that
includes `FlateDecode` with `Predictor`, `Columns`, and `Colors` parameters.
-}
module PDF.Processing.FilterCombine.PredDeflate
  ( predDeflate
  ) where

import Codec.Compression.Flate qualified as FL
import Codec.Compression.Predict
    ( Entropy (EntropyDeflate, EntropyShannon)
    , Predictor (PNGOptimum)
    , predict
    )

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Fallible (Fallible)
import Data.PDF.Filter (Filter (Filter))
import Data.PDF.FilterCombination (FilterCombination, mkFCAppend)
import Data.PDF.PDFObject (PDFObject (PDFName), mkPDFDictionary)
import Data.UnifiedError (UnifiedError (InvalidFilterParm))

import PDF.Object.Object.ToPDFNumber (mkPDFNumber)

{-|
Apply PNG predictor, then fast Deflate, selecting the better result between
Shannon and Deflate heuristics.

Requires `(width, components)`; returns `InvalidFilterParm` when width is
missing.
-}
predDeflate
  :: Maybe (Int, Int)
  -> ByteString
  -> Fallible FilterCombination
predDeflate (Just (width, components)) stream = do
  compressedS <-
    predict EntropyShannon PNGOptimum width components stream
      >>= FL.fastCompress

  compressedD <-
    predict EntropyDeflate PNGOptimum width components stream
      >>= FL.fastCompress

  let compressed = if BS.length compressedD < BS.length compressedS
        then compressedD
        else compressedS

  return $ mkFCAppend
    [ Filter
        (PDFName "FlateDecode")
        (mkPDFDictionary
          [ ("Predictor", mkPDFNumber PNGOptimum)
          , ("Columns"  , mkPDFNumber width)
          , ("Colors"   , mkPDFNumber components)
          ]
        )
    ]
    compressed

predDeflate _noWidth _stream = Left $
  InvalidFilterParm "no width given to predDeflate"
