{-|
Filter candidates for text and general non-image streams.

These usually compress well with Deflate/Zopfli. The module evaluates:

* No-op
* Deflate/Zopfli

and logs the comparison results for downstream selection.
-}
module PDF.Processing.ApplyFilter.Text
  ( applyEveryFilterText) where

import Control.Monad.State (gets, lift)
import Control.Monad.Trans.Except (except)

import Data.Bitmap.BitmapConfiguration (BitmapConfiguration)
import Data.ByteString (ByteString)
import Data.Logging (Logging)
import Data.PDF.FilterCombination (FilterCombination, fcBytes, mkFCAppend)
import Data.PDF.PDFWork (PDFWork)
import Data.PDF.Settings (sCompressor)
import Data.PDF.WorkData (wSettings)

import PDF.Processing.ApplyFilter.Helpers (filterInfoCompressor)
import PDF.Processing.FilterCombine.Compressor (compressor)

{-|
Evaluate filter candidates.

Ignores width/components; tries Zopfli/Deflate and a no-op.
-}
applyEveryFilterText
  :: Logging IO
  => Maybe BitmapConfiguration
  -> ByteString
  -> PDFWork IO [FilterCombination]
applyEveryFilterText _anyBitmapConfig stream = do
  useCompressor <- gets (sCompressor . wSettings)

  let rNothing = mkFCAppend [] stream

  rCompressor <- lift (except $ compressor Nothing stream useCompressor)
  filterInfoCompressor useCompressor "" stream (fcBytes rCompressor)

  return [rNothing, rCompressor]