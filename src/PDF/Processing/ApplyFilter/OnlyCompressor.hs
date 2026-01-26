{-|
Minimal filter candidates focused on Deflate/Zopfli and predictors.

Designed for cases where only Deflate-style compression should be considered.
Evaluated candidates:

* No-op
* Deflate/Zopfli/Brotli
* Predictor + Deflate/Zopfli/Brotli (when bitmap metadata is present)

All candidates are logged for human-readable comparison.
-}
module PDF.Processing.ApplyFilter.OnlyCompressor
  (applyEveryFilterOnlyCompressor) where


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
Evaluate filter candidates.

Ignores width/components; tries Zopfli/Deflate and a no-op.
-}
applyEveryFilterOnlyCompressor
  :: Logging IO
  => Maybe BitmapConfiguration
  -> ByteString
  -> PDFWork IO [FilterCombination]
applyEveryFilterOnlyCompressor (Just bitmapConfig) stream = do
  useCompressor <- gets (sCompressor . wSettings)

  let rNothing = mkFCAppend [] stream

  rCompressor <- lift (except $ compressor Nothing stream useCompressor)
  filterInfoCompressor useCompressor "" stream (fcBytes rCompressor)

  rTiffCompressor <- lift (except $ predCompressor (Just bitmapConfig) stream useCompressor)
  filterInfoCompressor useCompressor
                   (predictorLabel rTiffCompressor <> "/")
                   stream
                   (fcBytes rTiffCompressor)

  return [rNothing, rCompressor, rTiffCompressor]

applyEveryFilterOnlyCompressor Nothing stream = do
  useCompressor <- gets (sCompressor . wSettings)

  let rNothing = mkFCAppend [] stream

  rCompressor <- lift (except $ compressor Nothing stream useCompressor)
  filterInfoCompressor useCompressor "" stream (fcBytes rCompressor)

  return [rNothing, rCompressor]
