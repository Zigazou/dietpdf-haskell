module Pdf.Object.FilterCombine.PredRleZopfli
  ( predRleZopfli
  ) where

import Codec.Compression.Flate qualified as FL
import Codec.Compression.Predict
    ( Entropy (EntropyRLE)
    , Predictor (PNGOptimum)
    , predict
    )
import Codec.Compression.RunLength qualified as RL

import Data.ByteString qualified as BS
import Data.Functor ((<&>))

import Pdf.Object.Container (Filter (Filter))
import Pdf.Object.FilterCombine.FilterCombination
    ( FilterCombination
    , mkFCAppend
    )
import Pdf.Object.Object
    ( PDFObject (PDFName, PDFNull)
    , mkPDFDictionary
    , mkPDFNumber
    )

import Util.UnifiedError (UnifiedError (InvalidFilterParm))

predRleZopfli
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> Either UnifiedError FilterCombination
predRleZopfli (Just (width, components)) stream =
  -- Try finding optimal predictors with Deflate "entropy" function
  predict EntropyRLE PNGOptimum width components stream
    >>= FL.noCompress
    >>= RL.compress
    >>= FL.compress
    <&> mkFCAppend
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

predRleZopfli _noWidth _stream = Left InvalidFilterParm
