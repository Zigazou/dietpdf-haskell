{-|
Evaluate and compare generic filter combinations for arbitrary PDF streams.

This module builds candidate 'FilterCombination's using:

* No-op (append)
* Run-Length Encoding (@RLEDecode@)
* Deflate/Compressor (@FlateDecode@), optionally with predictors
* Predictor + Deflate/Compressor variants (PNG/TIFF)
* @RLEDecode@ combined with Deflate/Compressor when beneficial
* JPEG 2000 (@JPXDecode@) when width/components are known

Selection depends on available 'BitmapConfiguration': when present, more
image-aware candidates are considered (predictors and JPEG2000). When missing,
the set is restricted to RLE and Deflate/Compressor options.

All candidates are logged via helper functions so downstream code can choose
the best compression.
-}
module PDF.Processing.ApplyFilter.Generic
  ( applyEveryFilterGeneric ) where


import Control.Monad.State (gets, lift)
import Control.Monad.Trans.Except (except)

import Data.Bitmap.BitmapConfiguration
  ( BitmapConfiguration (BitmapConfiguration, bcBitsPerComponent, bcComponents, bcLineWidth)
  )
import Data.Bitmap.BitsPerComponent (BitsPerComponent (BC2Bits, BC4Bits))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ColorSpace (fromComponents)
import Data.Logging (Logging)
import Data.PDF.FilterCombination
  (FilterCombination, fcBytes, fcLength, mkFCAppend)
import Data.PDF.PDFWork (PDFWork)
import Data.PDF.Settings (sCompressor)
import Data.PDF.WorkData (wSettings)

import PDF.Processing.ApplyFilter.Helpers
  (filterInfo, filterInfoCompressor, predictorLabel)
import PDF.Processing.FilterCombine.Jpeg2k (jpeg2k)
import PDF.Processing.FilterCombine.PredRleCompressor (predRleCompressor)
import PDF.Processing.FilterCombine.PredCompressor (predCompressor)
import PDF.Processing.FilterCombine.Rle (rle)
import PDF.Processing.FilterCombine.RleCompressor (rleCompressor)
import PDF.Processing.FilterCombine.Compressor (compressor)

{-|
Evaluate all generic filter candidates for a stream.

Behavior depends on the presence of width/components metadata:

* When available, considers RLE, Compressor/Deflate, predictor variants, and
  JPEG2000; optionally combines RLE with Compressor/Deflate when beneficial.
* When missing, limits to RLE and Compressor/Deflate options.

Returns candidate `FilterCombination`s to compare downstream.
-}
applyEveryFilterGeneric
  :: Logging IO
  => Bool
  -> Maybe BitmapConfiguration
  -> ByteString
  -> PDFWork IO [FilterCombination]
applyEveryFilterGeneric objectIsAMask (Just bitmapConfig) stream = do
  useCompressor <- gets (sCompressor . wSettings)

  let rNothing = mkFCAppend [] stream
      width = bcLineWidth bitmapConfig
      components = bcComponents bitmapConfig
      jpeg2kParameters = Just ( width
                              , BS.length stream `div` (width * components)
                              , fromComponents components
                              )
      quality = if objectIsAMask then Just 50 else Nothing

  rJpeg2k <- jpeg2k quality jpeg2kParameters stream
  filterInfo "JPEG2000" stream (fcBytes rJpeg2k)

  rRle <- lift (except $ rle (Just bitmapConfig) stream)
  filterInfo "RLE" stream (fcBytes rRle)

  rCompressor <- lift (except $ compressor (Just bitmapConfig) stream useCompressor)
  filterInfoCompressor useCompressor "" stream (fcBytes rCompressor)

  rleCombine <- if fcLength rRle < BS.length stream
    then do
      rRleCompressor <- lift (except $ rleCompressor (Just bitmapConfig) stream useCompressor)
      filterInfoCompressor useCompressor "RLE+" stream (fcBytes rRleCompressor)

      return [rRle, rRleCompressor]
    else
      return []

  rPredCompressor <- lift (except $ predCompressor (Just bitmapConfig) stream useCompressor)
  filterInfoCompressor useCompressor
                   (predictorLabel rPredCompressor <> "/")
                   stream
                   (fcBytes rPredCompressor)

  rPredRleCompressor <- lift (except $ predRleCompressor (Just bitmapConfig) stream useCompressor)
  filterInfoCompressor useCompressor
                   (predictorLabel rPredRleCompressor <> "/RLE+")
                   stream
                   (fcBytes rPredRleCompressor)

  return $ [rNothing, rCompressor, rPredCompressor, rPredRleCompressor, rJpeg2k]
        ++ rleCombine

applyEveryFilterGeneric _objectIsAMask Nothing stream = do
  useCompressor <- gets (sCompressor . wSettings)

  let
    rNothing = mkFCAppend [] stream
    b4bits1comp = BitmapConfiguration
      { bcLineWidth        = 2 * BS.length stream
      , bcComponents       = 1
      , bcBitsPerComponent = BC4Bits
      }
    b2bits1comp = BitmapConfiguration
      { bcLineWidth        = 4 * BS.length stream
      , bcComponents       = 1
      , bcBitsPerComponent = BC2Bits
      }

  rRle <- lift (except $ rle Nothing stream)
  filterInfo "RLE" stream (fcBytes rRle)

  rCompressor <- lift (except $ compressor Nothing stream useCompressor)
  filterInfoCompressor useCompressor "" stream (fcBytes rCompressor)

  rPredCompressor <- lift (except $ predCompressor Nothing stream useCompressor)
  filterInfoCompressor useCompressor
                   (predictorLabel rPredCompressor)
                   stream
                   (fcBytes rPredCompressor)

  rPred4Compressor <- lift (except $ predCompressor (Just b4bits1comp) stream useCompressor)
  filterInfoCompressor useCompressor
                   (predictorLabel rPred4Compressor <> "4/")
                   stream
                   (fcBytes rPred4Compressor)

  rPred2Compressor <- lift (except $ predCompressor (Just b2bits1comp) stream useCompressor)
  filterInfoCompressor useCompressor
                   (predictorLabel rPred2Compressor <> "2/")
                   stream
                   (fcBytes rPred2Compressor)

  if fcLength rRle < BS.length stream
    then do
      rRleCompressor <- lift (except $ rleCompressor Nothing stream useCompressor)
      filterInfoCompressor useCompressor "RLE+" stream (fcBytes rRleCompressor)

      return [ rNothing
             , rRle
             , rCompressor
             , rRleCompressor
             , rPredCompressor
             , rPred4Compressor
             , rPred2Compressor
             ]
    else
      return [ rNothing
             , rCompressor
             , rPredCompressor
             , rPred4Compressor
             , rPred2Compressor
             ]
