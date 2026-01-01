{-|
Predictor + LZW filter combination.

Finds an optimal PNG predictor using both Shannon and Deflate-oriented
heuristics, compresses with LZW, and returns a `FilterCombination` with
`LZWDecode` and predictor parameters.
-}
module PDF.Processing.FilterCombine.PredLzw
  ( predLzw
  ) where
import Codec.Compression.LZW qualified as LZW
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
Apply PNG predictor, then LZW compression, selecting the better result between
Shannon and Deflate heuristics.

Requires `(width, components)`; returns `InvalidFilterParm` when width is
missing.
-}
predLzw
  :: Maybe (Int, Int)
  -> ByteString
  -> Fallible FilterCombination
predLzw (Just (width, components)) stream = do
  compressedS <-
    predict EntropyShannon PNGOptimum width components stream >>= LZW.compress

  compressedD <-
    predict EntropyDeflate PNGOptimum width components stream >>= LZW.compress

  let compressed = if BS.length compressedD < BS.length compressedS
        then compressedD
        else compressedS

  return $ mkFCAppend
    [ Filter
        (PDFName "LZWDecode")
        (mkPDFDictionary
          [ ("Predictor", mkPDFNumber PNGOptimum)
          , ("Columns"  , mkPDFNumber width)
          , ("Colors"   , mkPDFNumber components)
          ]
        )
    ]
    compressed

predLzw _noWidth _stream = Left
  $ InvalidFilterParm "no width given to predLzw"
