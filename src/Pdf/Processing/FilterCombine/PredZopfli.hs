module Pdf.Processing.FilterCombine.PredZopfli
  ( predZopfli
  ) where

import Codec.Compression.Flate qualified as FL
import Codec.Compression.Predict
    ( Entropy (EntropyDeflate, EntropyShannon)
    , Predictor (PNGOptimum, TIFFPredictor2)
    , predict
    )

import Data.ByteString qualified as BS
import Data.Fallible (Fallible)
import Data.Functor ((<&>))
import Data.PDF.Filter (Filter (Filter))
import Data.PDF.FilterCombination (FilterCombination, mkFCAppend)
import Data.PDF.PDFObject (PDFObject (PDFName), mkPDFDictionary)

import Pdf.Object.Object.ToPDFNumber (mkPDFNumber)

predZopfli
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> Fallible FilterCombination
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

  return $ mkFCAppend
    [ Filter
        (PDFName "FlateDecode")
        (mkPDFDictionary
          [ ("Predictor", mkPDFNumber PNGOptimum)
          , ("Columns"  , mkPDFNumber width)
          , ("Colors"   , mkPDFNumber components)
          ]
        )
    ]
    compressed

predZopfli Nothing stream =  do
  let
    width      = BS.length stream
    components = 1 :: Int

  predict EntropyShannon
          TIFFPredictor2
          width
          components
          stream
    >>= FL.compress
    <&> mkFCAppend
          [ Filter
              (PDFName "FlateDecode")
              (mkPDFDictionary
                [ ("Predictor", mkPDFNumber TIFFPredictor2)
                , ("Columns"  , mkPDFNumber width)
                , ("Colors"   , mkPDFNumber components)
                ]
              )
          ]
