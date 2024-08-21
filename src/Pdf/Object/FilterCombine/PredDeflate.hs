module Pdf.Object.FilterCombine.PredDeflate
  ( predDeflate
  ) where

import Codec.Compression.Flate qualified as FL
import Codec.Compression.Predictor
    ( EntropyType (EntropyDeflate, EntropyShannon)
    , Predictor (PNGOptimum)
    , predict
    )

import Data.ByteString qualified as BS

import Pdf.Object.Container (Filter (Filter), FilterList)
import Pdf.Object.Object (PDFObject (PDFName), mkPDFDictionary, mkPDFNumber)

import Util.Array (mkArray)
import Util.UnifiedError (UnifiedError (InvalidFilterParm))

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
