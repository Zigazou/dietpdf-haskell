module PDF.Processing.FilterCombine.PredRleZopfli
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
import Data.Fallible (Fallible)
import Data.Functor ((<&>))
import Data.PDF.Filter (Filter (Filter))
import Data.PDF.FilterCombination (FilterCombination, mkFCAppend)
import Data.PDF.PDFObject (PDFObject (PDFName, PDFNull), mkPDFDictionary)
import Data.PDF.Settings (UseZopfli (UseDeflate, UseZopfli))
import Data.UnifiedError (UnifiedError (InvalidFilterParm))

import PDF.Object.Object.ToPDFNumber (mkPDFNumber)

getCompressor :: UseZopfli -> (BS.ByteString -> Fallible BS.ByteString)
getCompressor UseZopfli  = FL.compress
getCompressor UseDeflate = FL.fastCompress

predRleZopfli
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> UseZopfli
  -> Fallible FilterCombination
predRleZopfli (Just (width, components)) stream useZopfli = do
  let compressor = getCompressor useZopfli

  -- Try finding optimal predictors with Deflate "entropy" function
  predict EntropyRLE PNGOptimum width components stream
    >>= FL.noCompress
    >>= RL.compress
    >>= compressor
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

predRleZopfli _noWidth _stream _useZopfli = Left InvalidFilterParm
