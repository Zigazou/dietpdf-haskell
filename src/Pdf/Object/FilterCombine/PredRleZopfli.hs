module Pdf.Object.FilterCombine.PredRleZopfli
  ( predRleZopfli
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
import qualified Codec.Compression.RunLength   as RL
import qualified Data.ByteString               as BS
import qualified Data.Map.Strict               as Map
import           Pdf.Object.Container           ( Filter(Filter)
                                                , FilterList
                                                )
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFDictionary
                                                  , PDFName
                                                  , PDFNumber
                                                  , PDFNull
                                                  )
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
        (PDFDictionary
          (Map.fromList
            [ ("Predictor", PDFNumber (fromIntegral . toWord8 $ PNGOptimum))
            , ("Columns"  , PDFNumber (fromIntegral width))
            , ("Colors"   , PDFNumber (fromIntegral components))
            ]
          )
        )
      ]
    , predicted
    )
predRleZopfli _noWidth _stream = Left InvalidFilterParm
