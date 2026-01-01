{-|
Predictor + Zopfli/Deflate filter combination.

Finds an optimal PNG predictor using both Shannon and Deflate-oriented
heuristics, then compresses with either Zopfli or fast Deflate, returning a
`FilterCombination` with predictor parameters. When width is unknown, uses
`TIFFPredictor2` with width set to stream length and components to 1.
-}
module PDF.Processing.FilterCombine.PredZopfli
  ( predZopfli
  ) where

import Codec.Compression.Flate qualified as FL
import Codec.Compression.Predict
    ( Entropy (EntropyDeflate, EntropyShannon)
    , Predictor (PNGOptimum, TIFFPredictor2)
    , predict
    )

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Fallible (Fallible)
import Data.Functor ((<&>))
import Data.PDF.Filter (Filter (Filter))
import Data.PDF.FilterCombination (FilterCombination, mkFCAppend)
import Data.PDF.PDFObject (PDFObject (PDFName), mkPDFDictionary)
import Data.PDF.Settings (UseZopfli (UseDeflate, UseZopfli))

import PDF.Object.Object.ToPDFNumber (mkPDFNumber)

getCompressor :: UseZopfli -> (ByteString -> Fallible ByteString)
getCompressor UseZopfli  = FL.compress
getCompressor UseDeflate = FL.fastCompress

{-|
Apply PNG predictor, then Zopfli/Deflate, selecting the better result between
Shannon and Deflate heuristics. If `(width, components)` is missing, falls back
to `TIFFPredictor2`.
-}
predZopfli
  :: Maybe (Int, Int)
  -> ByteString
  -> UseZopfli
  -> Fallible FilterCombination
predZopfli (Just (width, components)) stream useZopfli = do
  let compressor = getCompressor useZopfli

  compressedS <-
    predict EntropyShannon PNGOptimum width components stream >>= compressor

  compressedD <-
    predict EntropyDeflate PNGOptimum width components stream >>= compressor

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

predZopfli Nothing stream useZopfli =  do
  let
    compressor = getCompressor useZopfli
    width      = BS.length stream
    components = 1 :: Int

  predict EntropyShannon
          TIFFPredictor2
          width
          components
          stream
    >>= compressor
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
