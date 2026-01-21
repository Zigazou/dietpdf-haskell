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
import Codec.Compression.Predict.Entropy (Entropy (EntropyLFS, EntropySum))

import Data.Bitmap.BitmapConfiguration
  ( BitmapConfiguration (bcBitsPerComponent, bcComponents, bcLineWidth)
  , findBitmapConfigurations
  )
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Fallible (Fallible)
import Data.Functor ((<&>))
import Data.List (minimumBy)
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
  :: Maybe BitmapConfiguration
  -> ByteString
  -> UseZopfli
  -> Fallible FilterCombination
predZopfli (Just bitmapConfig) stream useZopfli = do
  let compressor = getCompressor useZopfli
      width      = bcLineWidth bitmapConfig
      components = bcComponents bitmapConfig

  -- Select entropies based on width.
  let entropies = if width < 64
                    then [EntropyShannon, EntropyLFS, EntropySum]
                    else [EntropyDeflate]

  -- Try all entropies and select the best compressed result.
  pngCompressed <- mapM (\entropy ->
                        predict entropy PNGOptimum bitmapConfig stream
                        >>= compressor
                      ) entropies
                   <&> minimumBy (\a b -> BS.length a `compare` BS.length b)

  -- Also try TIFF predictor.
  tiffCompressed <- predict EntropyShannon TIFFPredictor2 bitmapConfig stream
                     >>= compressor

  let (predictor, compressed) =
        if BS.length pngCompressed < BS.length tiffCompressed
          then (PNGOptimum, pngCompressed)
          else (TIFFPredictor2, tiffCompressed)

  return $ mkFCAppend
    [ Filter
        (PDFName "FlateDecode")
        (mkPDFDictionary
          [ ("Predictor", mkPDFNumber predictor)
          , ("Columns"  , mkPDFNumber width)
          , ("Colors"   , mkPDFNumber components)
          ]
        )
    ]
    compressed

predZopfli Nothing stream useZopfli =  do
  let
    compressor = getCompressor useZopfli
    possibleConfigs = findBitmapConfigurations (BS.length stream)

  (bitmapConfig, compressed) <- mapM (\bitmapConfig ->
                        predict EntropyShannon TIFFPredictor2 bitmapConfig stream
                        >>= compressor >>= \c -> return (bitmapConfig, c)
                      ) possibleConfigs
                   <&> minimumBy (\(_, a) (_, b) -> BS.length a `compare` BS.length b)

  return $ mkFCAppend
          [ Filter
              (PDFName "FlateDecode")
              (mkPDFDictionary
                [ ("Predictor", mkPDFNumber TIFFPredictor2)
                , ("Columns"  , mkPDFNumber $ bcLineWidth bitmapConfig)
                , ("Colors"   , mkPDFNumber $ bcComponents bitmapConfig)
                , ("BitsPerComponent", mkPDFNumber . fromEnum $ bcBitsPerComponent bitmapConfig)
                ]
              )
          ]
          compressed
