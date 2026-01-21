{-|
Helpers for logging filter application results and labeling predictor types.

Provides:

* 'filterInfo' — logs before/after sizes for any filter.
* 'filterInfoZopfli' — prefixes the log with “Zopfli” or “Deflate”
  depending on 'UseZopfli'.
* 'predictorLabel' — returns a short text identifying the first predictor
  in a combination (PNG/TIFF group).

These helpers are intended for human-readable comparison output while
evaluating compression strategies.
-}
module PDF.Processing.ApplyFilter.Helpers
  (predictorLabel, filterInfoZopfli, filterInfo) where

import Codec.Compression.Predict.Predictor (isPNGGroup)

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Logging (Logging)
import Data.PDF.FilterCombination (FilterCombination, firstPredictor)
import Data.PDF.PDFWork (PDFWork, sayComparisonP)
import Data.PDF.Settings (UseZopfli (UseDeflate, UseZopfli))
import Data.Text (Text)
import Data.Text qualified as T

{-|
Log a comparison line for a filter application, showing the stream size before
and after applying a given filter.
-}
filterInfo
  :: Logging m
  => T.Text
  -> ByteString
  -> ByteString
  -> PDFWork m ()
filterInfo filterName streamBefore streamAfter =
  sayComparisonP ("filter " <> filterName)
                 (BS.length streamBefore)
                 (BS.length streamAfter)


{-|
Log a comparison line for Zopfli/Deflate filter application, depending on the
`UseZopfli` setting.
-}
filterInfoZopfli
  :: Logging m
  => UseZopfli
  -> T.Text
  -> ByteString
  -> ByteString
  -> PDFWork m ()
filterInfoZopfli UseZopfli filterName streamBefore streamAfter =
  filterInfo (filterName <> "Zopfli") streamBefore streamAfter
filterInfoZopfli UseDeflate filterName streamBefore streamAfter =
  filterInfo (filterName <> "Deflate") streamBefore streamAfter

{-|
Generate a prefix string for logging based on the first predictor in the
filter combination.
-}
predictorLabel :: FilterCombination -> Text
predictorLabel filterCombination =
  case firstPredictor filterCombination of
    Just predictor -> if isPNGGroup predictor
                        then "PredictorPNG"
                        else "PredictorTIFF"
    Nothing        -> "Predictor"
