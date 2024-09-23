module Pdf.Object.FilterCombine.PredLzw
  ( predLzw
  ) where
import Codec.Compression.LZW qualified as LZW
import Codec.Compression.Predictor
    ( EntropyType (EntropyDeflate, EntropyShannon)
    , Predictor (PNGOptimum)
    , predict
    )

import Data.ByteString qualified as BS

import Pdf.Object.Container (Filter (Filter))
import Pdf.Object.FilterCombine.FilterCombination
    ( FilterCombination
    , mkFCAppend
    )
import Pdf.Object.Object (PDFObject (PDFName), mkPDFDictionary, mkPDFNumber)

import Util.UnifiedError (UnifiedError (InvalidFilterParm))

predLzw
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> Either UnifiedError FilterCombination
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
