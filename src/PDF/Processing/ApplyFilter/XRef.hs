{-|
Filter candidates for cross-reference (XRef) streams.

When field widths/components are known, applying a predictor prior to
Deflate/Zopfli often yields better results.

Evaluated candidates:

* Predictor + Deflate/Zopfli (when bitmap metadata is present)
* No-op and Deflate/Zopfli otherwise

All candidates are logged for later comparison.
-}
module PDF.Processing.ApplyFilter.XRef
  ( applyEveryFilterXRef ) where

import Control.Monad.State (gets, lift)
import Control.Monad.Trans.Except (except)

import Data.Bitmap.BitmapConfiguration (BitmapConfiguration)
import Data.ByteString (ByteString)
import Data.Logging (Logging)
import Data.PDF.FilterCombination (FilterCombination, fcBytes, mkFCAppend)
import Data.PDF.PDFWork (PDFWork)
import Data.PDF.Settings (sCompressor)
import Data.PDF.WorkData (wSettings)

import PDF.Processing.ApplyFilter.Helpers (filterInfoCompressor, predictorLabel)
import PDF.Processing.FilterCombine.Compressor (compressor)
import PDF.Processing.FilterCombine.PredCompressor (predCompressor)

{-|
Evaluate filter candidates for XRef streams.

Prefers Predictor + Compressor when width/components are known; otherwise
offers a no-op and Compressor.
-}
applyEveryFilterXRef
  :: Logging m
  => Maybe BitmapConfiguration
  -> ByteString
  -> PDFWork m [FilterCombination]
applyEveryFilterXRef (Just bitmapConfig) stream = do
  useCompressor <- gets (sCompressor . wSettings)
  rPredCompressor <- lift (except $ predCompressor (Just bitmapConfig) stream useCompressor)
  filterInfoCompressor useCompressor
                   (predictorLabel rPredCompressor <> "/")
                   stream
                   (fcBytes rPredCompressor)

  return [rPredCompressor]

applyEveryFilterXRef Nothing stream = do
  useCompressor <- gets (sCompressor . wSettings)
  let rNothing = mkFCAppend [] stream

  rCompressor <- lift (except $ compressor Nothing stream useCompressor)
  filterInfoCompressor useCompressor "" stream (fcBytes rCompressor)

  return [rNothing, rCompressor]