{-|
Filter candidates for ICC profile streams.

ICC data often benefits from Deflate/Zopfli with a simple 2-component,
8-bit predictor. This module evaluates and logs:

* No-op
* Deflate/Zopfli
* Predictor (2 components, 8 bits) + Deflate/Zopfli

and returns candidates for downstream comparison.
-}
module PDF.Processing.ApplyFilter.ICC
  (applyEveryFilterICC) where

import Control.Monad.State (gets, lift)
import Control.Monad.Trans.Except (except)

import Data.Bitmap.BitmapConfiguration
  ( BitmapConfiguration (BitmapConfiguration, bcBitsPerComponent, bcComponents, bcLineWidth)
  )
import Data.Bitmap.BitsPerComponent (BitsPerComponent (BC8Bits))
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
Evaluate filter candidates specifically for ICC profile streams.
-}
applyEveryFilterICC
  :: Logging IO
  => Maybe BitmapConfiguration
  -> ByteString
  -> PDFWork IO [FilterCombination]
applyEveryFilterICC _anyBitmapConfig stream = do
  useZopfli <- gets (sZopfli . wSettings)

  let rNothing = mkFCAppend [] stream

  rZopfli <- lift (except $ zopfli Nothing stream useZopfli)
  filterInfoZopfli useZopfli "" stream (fcBytes rZopfli)

  let bitmapConfig = BitmapConfiguration
        { bcLineWidth        = 1
        , bcComponents       = 2
        , bcBitsPerComponent = BC8Bits
        }

  rPred2Zopfli <- lift (except $ predZopfli (Just bitmapConfig) stream useZopfli)
  filterInfoZopfli useZopfli
                   (predictorLabel rPred2Zopfli <> "2/")
                   stream
                   (fcBytes rPred2Zopfli)

  return [rNothing, rZopfli, rPred2Zopfli]
