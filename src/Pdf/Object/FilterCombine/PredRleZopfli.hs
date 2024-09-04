module Pdf.Object.FilterCombine.PredRleZopfli
  ( predRleZopfli
  ) where

import Codec.Compression.Flate qualified as FL
import Codec.Compression.Predictor
    ( EntropyType (EntropyRLE)
    , Predictor (PNGOptimum)
    , predict
    )
import Codec.Compression.RunLength qualified as RL

import Data.ByteString qualified as BS

import Pdf.Object.Container (Filter (Filter), FilterList)
import Pdf.Object.Object
    ( PDFObject (PDFName, PDFNull)
    , mkPDFDictionary
    , mkPDFNumber
    )

import Util.Array (mkArray)
import Util.UnifiedError (UnifiedError (InvalidFilterParm))

predRleZopfli
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> Either UnifiedError (FilterList, BS.ByteString)
predRleZopfli (Just (width, components)) stream = do
  -- Try finding optimal predictors with Deflate "entropy" function
  predicted <-
    predict EntropyRLE PNGOptimum width components stream
    >>= FL.noCompress
    >>= RL.compress
    >>= FL.compress

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
