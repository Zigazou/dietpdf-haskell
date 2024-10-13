module Pdf.Processing.FilterCombine.PredLzw
  ( predLzw
  ) where
import Codec.Compression.LZW qualified as LZW
import Codec.Compression.Predict
    ( Entropy (EntropyDeflate, EntropyShannon)
    , Predictor (PNGOptimum)
    , predict
    )

import Data.ByteString qualified as BS
import Data.Fallible (Fallible)
import Data.PDF.Filter (Filter (Filter))
import Data.PDF.FilterCombination (FilterCombination, mkFCAppend)
import Data.PDF.PDFObject (PDFObject (PDFName), mkPDFDictionary)
import Data.UnifiedError (UnifiedError (InvalidFilterParm))

import Pdf.Object.Object.ToPDFNumber (mkPDFNumber)

predLzw
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> Fallible FilterCombination
predLzw (Just (width, components)) stream = do
  -- Try finding optimal predictors with Shannon entropy function
  compressedS <-
    predict EntropyShannon PNGOptimum width components stream >>= LZW.compress

  -- Try finding optimal predictors with Deflate "entropy" function
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

predLzw _noWidth _stream = Left InvalidFilterParm
