module Pdf.Object.FilterCombine.PredRleZopfli
  ( predRleZopfli
  ) where

import qualified Codec.Compression.Flate       as FL
import           Codec.Compression.Predictor    ( Predictor(PNGOptimum)
                                                , predict
                                                , EntropyType
                                                  ( EntropyShannon
                                                  , EntropyDeflate
                                                  )
                                                )
import qualified Codec.Compression.RunLength   as RL
import qualified Data.ByteString               as BS
import           Pdf.Object.Container           ( Filter(Filter)
                                                , FilterList
                                                )
import           Pdf.Object.Object              ( PDFObject(PDFName, PDFNull)
                                                , mkPDFDictionary
                                                , mkPDFNumber
                                                )
import           Util.UnifiedError              ( UnifiedError
                                                  ( InvalidFilterParm
                                                  )
                                                )
import           Util.Array                     ( mkArray )

predRleZopfli
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> Either UnifiedError (FilterList, BS.ByteString)
predRleZopfli (Just (width, components)) stream = do
  -- Try finding optimal predictors with Shannon entropy function
  predictedS <-
    predict EntropyShannon PNGOptimum width components stream
    >>= FL.noCompress
    >>= RL.compress
    >>= FL.compress

  -- Try finding optimal predictors with Deflate "entropy" function
  predictedD <-
    predict EntropyDeflate PNGOptimum width components stream
    >>= FL.noCompress
    >>= RL.compress
    >>= FL.compress

  let predicted = if BS.length predictedD < BS.length predictedS
        then predictedD
        else predictedS

  return
    ( mkArray
      [ Filter (PDFName "FlateDecode")     PDFNull
      , Filter (PDFName "RunLengthDecode") PDFNull
      , Filter
        (PDFName "FlateDecode")
        (mkPDFDictionary
          [ ("Predictor", mkPDFNumber PNGOptimum)
          , ("Columns"  , mkPDFNumber width)
          , ("Colors"   , mkPDFNumber components)
          ]
        )
      ]
    , predicted
    )

predRleZopfli _noWidth _stream = Left InvalidFilterParm
