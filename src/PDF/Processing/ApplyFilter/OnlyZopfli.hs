{-|
Minimal filter candidates focused on Deflate/Zopfli and predictors.

Designed for cases where only Deflate-style compression should be considered.
Evaluated candidates:

* No-op
* Deflate/Zopfli
* Predictor + Deflate/Zopfli (when bitmap metadata is present)

All candidates are logged for human-readable comparison.
-}
module PDF.Processing.ApplyFilter.OnlyZopfli
  ( applyEveryFilterOnlyZopfi) where


import Control.Monad.State (gets, lift)
import Control.Monad.Trans.Except (except)

import Data.Bitmap.BitmapConfiguration (BitmapConfiguration)
import Data.ByteString (ByteString)
import Data.Logging (Logging)
import Data.PDF.FilterCombination (FilterCombination, fcBytes, mkFCAppend)
import Data.PDF.PDFWork (PDFWork)
import Data.PDF.Settings (sZopfli)
import Data.PDF.WorkData (wSettings)

import PDF.Processing.ApplyFilter.Helpers (filterInfoZopfli, predictorLabel)
import PDF.Processing.FilterCombine.PredZopfli (predZopfli)
import PDF.Processing.FilterCombine.Zopfli (zopfli)

{-|
Evaluate filter candidates.

Ignores width/components; tries Zopfli/Deflate and a no-op.
-}
applyEveryFilterOnlyZopfi
  :: Logging IO
  => Maybe BitmapConfiguration
  -> ByteString
  -> PDFWork IO [FilterCombination]
applyEveryFilterOnlyZopfi (Just bitmapConfig) stream = do
  useZopfli <- gets (sZopfli . wSettings)

  let rNothing = mkFCAppend [] stream

  rZopfli <- lift (except $ zopfli Nothing stream useZopfli)
  filterInfoZopfli useZopfli "" stream (fcBytes rZopfli)

  rTiffZopfli <- lift (except $ predZopfli (Just bitmapConfig) stream useZopfli)
  filterInfoZopfli useZopfli
                   (predictorLabel rTiffZopfli <> "/")
                   stream
                   (fcBytes rTiffZopfli)

  return [rNothing, rZopfli, rTiffZopfli]

applyEveryFilterOnlyZopfi Nothing stream = do
  useZopfli <- gets (sZopfli . wSettings)

  let rNothing = mkFCAppend [] stream

  rZopfli <- lift (except $ zopfli Nothing stream useZopfli)
  filterInfoZopfli useZopfli "" stream (fcBytes rZopfli)

  return [rNothing, rZopfli]
