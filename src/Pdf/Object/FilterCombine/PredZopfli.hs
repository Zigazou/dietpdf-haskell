module Pdf.Object.FilterCombine.PredZopfli
  ( predZopfli
  ) where

import Codec.Compression.Flate qualified as FL
import Codec.Compression.Predictor
    ( EntropyType (EntropyDeflate, EntropyShannon)
    , Predictor (PNGOptimum, TIFFPredictor2)
    , predict
    )

import Data.ByteString qualified as BS

import Pdf.Object.Container (Filter (Filter), FilterList)
import Pdf.Object.Object (PDFObject (PDFName), mkPDFDictionary, mkPDFNumber)

import Util.Array (mkArray)
import Util.UnifiedError (UnifiedError)

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
          (mkPDFDictionary
            [ ("Predictor", mkPDFNumber PNGOptimum)
            , ("Columns"  , mkPDFNumber width)
            , ("Colors"   , mkPDFNumber components)
            ]
          )
      ]
    , compressed
    )

predZopfli Nothing stream =  do
  let
    width      = BS.length stream
    components = 1 :: Int

  compressed <- predict EntropyShannon
                        TIFFPredictor2
                        width
                        components
                        stream
                    >>= FL.compress

  return
    ( mkArray
      [ Filter
          (PDFName "FlateDecode")
          (mkPDFDictionary
            [ ("Predictor", mkPDFNumber TIFFPredictor2)
            , ("Columns"  , mkPDFNumber width)
            , ("Colors"   , mkPDFNumber components)
            ]
          )
      ]
    , compressed
    )
