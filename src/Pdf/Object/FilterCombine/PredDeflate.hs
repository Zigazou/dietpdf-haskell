module Pdf.Object.FilterCombine.PredDeflate
  ( predDeflate
  ) where

import qualified Codec.Compression.Flate       as FL
import           Codec.Compression.Predictor    ( Predictor(PNGOptimum)
                                                , predict
                                                , EntropyType
                                                  ( EntropyShannon
                                                  , EntropyDeflate
                                                  )
                                                )
import qualified Data.ByteString               as BS
import           Pdf.Object.Container           ( Filter(Filter)
                                                , FilterList
                                                )
import           Pdf.Object.Object              ( PDFObject(PDFName)
                                                , mkPDFDictionary
                                                , mkPDFNumber
                                                )
import           Util.UnifiedError              ( UnifiedError
                                                  ( InvalidFilterParm
                                                  )
                                                )
import           Util.Array                     ( mkArray )

predDeflate
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> Either UnifiedError (FilterList, BS.ByteString)
predDeflate (Just (width, components)) stream = do
  -- Try finding optimal predictors with Shannon entropy function
  compressedS <-
    predict EntropyShannon PNGOptimum width components stream
      >>= FL.fastCompress

  -- Try finding optimal predictors with Deflate "entropy" function
  compressedD <-
    predict EntropyDeflate PNGOptimum width components stream
      >>= FL.fastCompress

  let compressed = if BS.length compressedD < BS.length compressedS
        then compressedD
        else compressedS

  return
    ( mkArray
      [ Filter
          (PDFName "FlateDecode")
          (mkPDFDictionary
            [ ("Predictor", mkPDFNumber PNGOptimum)
            , ("Columns"  , mkPDFNumber width)
            , ("Colors"   , mkPDFNumber components)
            ]
          )
      ]
    , compressed
    )

predDeflate _noWidth _stream = Left InvalidFilterParm
