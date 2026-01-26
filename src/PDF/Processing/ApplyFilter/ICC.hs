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
import Data.PDF.Settings (sCompressor)
import Data.PDF.WorkData (wSettings)

import PDF.Processing.ApplyFilter.Helpers (filterInfoCompressor, predictorLabel)
import PDF.Processing.FilterCombine.Compressor (compressor)
import PDF.Processing.FilterCombine.PredCompressor (predCompressor)

{-|
Evaluate filter candidates specifically for ICC profile streams.
-}
applyEveryFilterICC
  :: Logging IO
  => Maybe BitmapConfiguration
  -> ByteString
  -> PDFWork IO [FilterCombination]
applyEveryFilterICC _anyBitmapConfig stream = do
  useCompressor <- gets (sCompressor . wSettings)

  let rNothing = mkFCAppend [] stream

  rZopfli <- lift (except $ compressor Nothing stream useCompressor)
  filterInfoCompressor useCompressor "" stream (fcBytes rZopfli)

  let bitmapConfig = BitmapConfiguration
        { bcLineWidth        = 1
        , bcComponents       = 2
        , bcBitsPerComponent = BC8Bits
        }

  rPred2Compressor <- lift (except $ predCompressor (Just bitmapConfig) stream useCompressor)
  filterInfoCompressor useCompressor
                   (predictorLabel rPred2Compressor <> "2/")
                   stream
                   (fcBytes rPred2Compressor)

  return [rNothing, rZopfli, rPred2Compressor]
