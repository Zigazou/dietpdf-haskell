{-|
Evaluate and compare generic filter combinations for arbitrary PDF streams.

This module builds candidate 'FilterCombination's using:

* No-op (append)
* Run-Length Encoding (@RLEDecode@)
* Deflate/Zopfli (@FlateDecode@), optionally with predictors
* Predictor + Deflate/Zopfli variants (PNG/TIFF)
* @RLEDecode@ combined with Deflate/Zopfli when beneficial
* JPEG 2000 (@JPXDecode@) when width/components are known

Selection depends on available 'BitmapConfiguration': when present, more
image-aware candidates are considered (predictors and JPEG2000). When missing,
the set is restricted to RLE and Deflate/Zopfli options.

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
import Data.PDF.Settings (sZopfli)
import Data.PDF.WorkData (wSettings)

import PDF.Processing.ApplyFilter.Helpers
  (filterInfo, filterInfoZopfli, predictorLabel)
import PDF.Processing.FilterCombine.Jpeg2k (jpeg2k)
import PDF.Processing.FilterCombine.PredRleZopfli (predRleZopfli)
import PDF.Processing.FilterCombine.PredZopfli (predZopfli)
import PDF.Processing.FilterCombine.Rle (rle)
import PDF.Processing.FilterCombine.RleZopfli (rleZopfli)
import PDF.Processing.FilterCombine.Zopfli (zopfli)

{-|
Evaluate all generic filter candidates for a stream.

Behavior depends on the presence of width/components metadata:

* When available, considers RLE, Zopfli/Deflate, predictor variants, and
  JPEG2000; optionally combines RLE with Zopfli/Deflate when beneficial.
* When missing, limits to RLE and Zopfli/Deflate options.

Returns candidate `FilterCombination`s to compare downstream.
-}
applyEveryFilterGeneric
  :: Logging IO
  => Bool
  -> Maybe BitmapConfiguration
  -> ByteString
  -> PDFWork IO [FilterCombination]
applyEveryFilterGeneric objectIsAMask (Just bitmapConfig) stream = do
  useZopfli <- gets (sZopfli . wSettings)

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

  rZopfli <- lift (except $ zopfli (Just bitmapConfig) stream useZopfli)
  filterInfoZopfli useZopfli "" stream (fcBytes rZopfli)

  rleCombine <- if fcLength rRle < BS.length stream
    then do
      rRleZopfli <- lift (except $ rleZopfli (Just bitmapConfig) stream useZopfli)
      filterInfoZopfli useZopfli "RLE+" stream (fcBytes rRleZopfli)

      return [rRle, rRleZopfli]
    else
      return []

  rPredZopfli <- lift (except $ predZopfli (Just bitmapConfig) stream useZopfli)
  filterInfoZopfli useZopfli
                   (predictorLabel rPredZopfli <> "/")
                   stream
                   (fcBytes rPredZopfli)

  rPredRleZopfli <- lift (except $ predRleZopfli (Just bitmapConfig) stream useZopfli)
  filterInfoZopfli useZopfli
                   (predictorLabel rPredRleZopfli <> "/RLE+")
                   stream
                   (fcBytes rPredRleZopfli)

  return $ [rNothing, rZopfli, rPredZopfli, rPredRleZopfli, rJpeg2k]
        ++ rleCombine

applyEveryFilterGeneric _objectIsAMask Nothing stream = do
  useZopfli <- gets (sZopfli . wSettings)

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

  rZopfli <- lift (except $ zopfli Nothing stream useZopfli)
  filterInfoZopfli useZopfli "" stream (fcBytes rZopfli)

  rPredZopfli <- lift (except $ predZopfli Nothing stream useZopfli)
  filterInfoZopfli useZopfli
                   (predictorLabel rPredZopfli)
                   stream
                   (fcBytes rPredZopfli)

  rPred4Zopfli <- lift (except $ predZopfli (Just b4bits1comp) stream useZopfli)
  filterInfoZopfli useZopfli
                   (predictorLabel rPred4Zopfli <> "4/")
                   stream
                   (fcBytes rPred4Zopfli)

  rPred2Zopfli <- lift (except $ predZopfli (Just b2bits1comp) stream useZopfli)
  filterInfoZopfli useZopfli
                   (predictorLabel rPred2Zopfli <> "2/")
                   stream
                   (fcBytes rPred2Zopfli)

  if fcLength rRle < BS.length stream
    then do
      rRleZopfli <- lift (except $ rleZopfli Nothing stream useZopfli)
      filterInfoZopfli useZopfli "RLE+" stream (fcBytes rRleZopfli)

      return [ rNothing
             , rRle
             , rZopfli
             , rRleZopfli
             , rPredZopfli
             , rPred4Zopfli
             , rPred2Zopfli
             ]
    else
      return [ rNothing
             , rZopfli
             , rPredZopfli
             , rPred4Zopfli
             , rPred2Zopfli
             ]
