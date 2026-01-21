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
import Data.PDF.Settings (sZopfli)
import Data.PDF.WorkData (wSettings)

import PDF.Processing.ApplyFilter.Helpers (filterInfoZopfli, predictorLabel)
import PDF.Processing.FilterCombine.PredZopfli (predZopfli)
import PDF.Processing.FilterCombine.Zopfli (zopfli)

{-|
Evaluate filter candidates for XRef streams.

Prefers Predictor + Zopfli/Deflate when width/components are known; otherwise
offers a no-op and Zopfli/Deflate.
-}
applyEveryFilterXRef
  :: Logging m
  => Maybe BitmapConfiguration
  -> ByteString
  -> PDFWork m [FilterCombination]
applyEveryFilterXRef (Just bitmapConfig) stream = do
  useZopfli <- gets (sZopfli . wSettings)
  rPredZopfli <- lift (except $ predZopfli (Just bitmapConfig) stream useZopfli)
  filterInfoZopfli useZopfli
                   (predictorLabel rPredZopfli <> "/")
                   stream
                   (fcBytes rPredZopfli)

  return [rPredZopfli]

applyEveryFilterXRef Nothing stream = do
  useZopfli <- gets (sZopfli . wSettings)
  let rNothing = mkFCAppend [] stream

  rZopfli <- lift (except $ zopfli Nothing stream useZopfli)
  filterInfoZopfli useZopfli "" stream (fcBytes rZopfli)

  return [rNothing, rZopfli]
