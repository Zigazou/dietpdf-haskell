module Pdf.Object.FilterCombine.PredZopfli
  ( predZopfli
  ) where

import qualified Codec.Compression.Flate       as FL
import           Codec.Compression.Predictor    ( Predictor(PNGOptimum)
                                                , predict
                                                , toWord8
                                                , EntropyType
                                                  ( EntropyShannon
                                                  , EntropyDeflate
                                                  )
                                                )
import qualified Data.ByteString               as BS
import qualified Data.Map.Strict               as Map
import           Pdf.Object.Container           ( Filter(Filter)
                                                , FilterList
                                                )
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFDictionary
                                                  , PDFName
                                                  , PDFNumber
                                                  )
                                                )
import           Util.UnifiedError              ( UnifiedError
                                                  ( InvalidFilterParm
                                                  )
                                                )
import           Util.Array                     ( mkArray )

predZopfli
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> Either UnifiedError (FilterList, BS.ByteString)
predZopfli (Just (width, components)) stream = do
  -- Try finding optimal predictors with Shannon entropy function
  compressedS <-
    predict EntropyShannon PNGOptimum width components stream >>= FL.compress

  -- Try finding optimal predictors with Deflate "entropy" function
  compressedD <-
    predict EntropyDeflate PNGOptimum width components stream >>= FL.compress

  let compressed = if BS.length compressedD < BS.length compressedS
        then compressedD
        else compressedS

  return
    ( mkArray
      [ Filter
          (PDFName "FlateDecode")
          (PDFDictionary
            (Map.fromList
              [ ("Predictor", PDFNumber (fromIntegral . toWord8 $ PNGOptimum))
              , ("Columns"  , PDFNumber (fromIntegral width))
              , ("Colors"   , PDFNumber (fromIntegral components))
              ]
            )
          )
      ]
    , compressed
    )
predZopfli _noWidth _stream = Left InvalidFilterParm
