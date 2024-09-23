{-|
This modules implements several optimization techniques targeted at PDF objects.
-}
module Pdf.Object.Filter
  ( filterOptimize
  ) where

import Control.Monad.Trans.Except (except)

import Data.ByteString qualified as BS
import Data.Foldable (minimumBy)
import Data.Functor ((<&>))
import Data.Sequence ((><))
import Data.Text qualified as T

import External.PamToJpeg2k (jpegToJpeg2k)

import Pdf.Document.XRef (xrefStreamWidth)
import Pdf.Object.Container (Filter (Filter), getFilters, setFilters)
import Pdf.Object.FilterCombine.FilterCombination
    ( FilterCombination (FilterCombination)
    , fcBytes
    , fcLength
    , fcList
    , fcReplace
    , mkFCAppend
    , mkFCReplace
    )
import Pdf.Object.FilterCombine.Jpeg2k (jpeg2k)
import Pdf.Object.FilterCombine.PredRleZopfli (predRleZopfli)
import Pdf.Object.FilterCombine.PredZopfli (predZopfli)
import Pdf.Object.FilterCombine.Rle (rle)
import Pdf.Object.FilterCombine.RleZopfli (rleZopfli)
import Pdf.Object.FilterCombine.Zopfli (zopfli)
import Pdf.Object.Object
    ( PDFObject (PDFName, PDFNull, PDFNumber, PDFNumber, PDFXRefStream)
    , fromPDFObject
    , hasStream
    )
import Pdf.Object.OptimizationType
    ( OptimizationType (JPGOptimization, XRefStreamOptimization)
    )
import Pdf.Object.State (getStream, getValue, setStream)

import Util.Array (mkArray)
import Util.Logging (Logging, sayComparisonF)
import Util.UnifiedError (FallibleT)

filterInfo
  :: Logging m => T.Text -> BS.ByteString -> BS.ByteString -> FallibleT m ()
filterInfo filterName streamBefore streamAfter = sayComparisonF
  ("Filter " <> filterName)
  (BS.length streamBefore)
  (BS.length streamAfter)

applyEveryFilterGeneric
  :: Logging IO
  => Maybe (Int, Int)
  -> BS.ByteString
  -> FallibleT IO [FilterCombination]
-- The stream has width and components information.
applyEveryFilterGeneric widthComponents@(Just (width, components)) stream = do
  let rNothing = mkFCAppend [] stream
      jpeg2kParameters =
        Just ( width
             , BS.length stream `div` (width * components)
             , components
             , case components of
                3 -> "RGB"
                4 -> "CMYK"
                _ -> "GRAY"
             )

  rJpeg2k <- if components /= 4
    then do
      rJpeg2k' <- jpeg2k jpeg2kParameters stream
      filterInfo "JPEG2000" stream (fcBytes rJpeg2k')

      return [rJpeg2k']
    else
      return []

  rRle <- except $ rle widthComponents stream
  filterInfo "RLE" stream (fcBytes rRle)

  rZopfli <- except $ zopfli widthComponents stream
  filterInfo "Zopfli" stream (fcBytes rZopfli)

  rleCombine <- if fcLength rRle < BS.length stream
    then do
      rRleZopfli <- except $ rleZopfli widthComponents stream
      filterInfo "RLE+Zopfli" stream (fcBytes rRleZopfli)

      return [rRle, rRleZopfli]
    else
      return []

  rPredZopfli <- except $ predZopfli widthComponents stream
  filterInfo "Predictor/Zopfli" stream (fcBytes rPredZopfli)

  rPredRleZopfli <- except $ predRleZopfli widthComponents stream
  filterInfo "Predictor/Store+RLE+Zopfli" stream (fcBytes rPredRleZopfli)

  return $ [rNothing, rZopfli, rPredZopfli, rPredRleZopfli]
        ++ rleCombine
        ++ rJpeg2k

-- The stream has no width nor components information.
applyEveryFilterGeneric Nothing stream = do
  let rNothing = mkFCAppend [] stream

  rRle <- except $ rle Nothing stream
  filterInfo "RLE" stream (fcBytes rRle)

  rZopfli <- except $ zopfli Nothing stream
  filterInfo "Zopfli" stream (fcBytes rZopfli)

  if fcLength rRle < BS.length stream
    then do
      rRleZopfli <- except $ rleZopfli Nothing stream
      filterInfo "RLE+Zopfli" stream (fcBytes rRleZopfli)

      return [rNothing, rRle, rZopfli, rRleZopfli]
    else
      return [rNothing, rZopfli]

applyEveryFilterJPG
  :: Logging IO
  => Maybe (Int, Int)
  -> BS.ByteString
  -> FallibleT IO [FilterCombination]
-- The stream has width and components information.
applyEveryFilterJPG (Just (_width, components)) stream = do
  let rNothing = mkFCAppend [] stream

  rZopfli <- except $ zopfli Nothing stream
  filterInfo "Zopfli" stream (fcBytes rZopfli)

  -- Try Jpeg2000 for images with less than 4 components.
  rJpeg2k <- if components /= 4
    then do
      rJpeg2k' <- jpegToJpeg2k 15 stream
                  <&> mkFCReplace [Filter (PDFName "JPXDecode") PDFNull]

      filterInfo "JPEG2000" stream (fcBytes rJpeg2k')
      return [rJpeg2k']
    else
      return []

  return $ [rNothing, rZopfli] ++ rJpeg2k

applyEveryFilterJPG Nothing stream = do
  let rNothing = mkFCAppend [] stream

  rZopfli <- except $ zopfli Nothing stream
  filterInfo "Zopfli" stream (fcBytes rZopfli)

  return [rNothing, rZopfli]

applyEveryFilterXRef
  :: Logging m
  => Maybe (Int, Int)
  -> BS.ByteString
  -> FallibleT m [FilterCombination]
-- The stream has width and components information.
applyEveryFilterXRef widthComponents@(Just (_width, _components)) stream = do
  rPredZopfli <- except $ predZopfli widthComponents stream
  filterInfo "Predictor+Zopfli" stream (fcBytes rPredZopfli)
  return [rPredZopfli]
applyEveryFilterXRef Nothing stream = do
  let rNothing = mkFCAppend [] stream

  rZopfli <- except $ zopfli Nothing stream
  filterInfo "Zopfli" stream (fcBytes rZopfli)
  return [rNothing, rZopfli]

getWidthComponents :: Logging m => PDFObject -> FallibleT m (Maybe (Int, Int))
getWidthComponents object@PDFXRefStream{} =
  xrefStreamWidth object <&> Just . (, 1)

getWidthComponents object = do
  width      <- getValue "Width" object
  colorSpace <- getValue "ColorSpace" object

  let components :: Int
      components = case colorSpace of
        Just (PDFName "DeviceRGB" ) -> 3
        Just (PDFName "DeviceCMYK") -> 4
        _anyOtherValue              -> 1

  case width of
    Just (PDFNumber width') -> return $ Just (round width', components)
    _anyOtherValue          -> return Nothing

filterOptimize
  :: Logging IO
  => OptimizationType
  -> PDFObject
  -> FallibleT IO PDFObject
filterOptimize optimization object = if hasStream object
  then do
    stream          <- getStream object
    filters         <- getFilters object
    widthComponents <- getWidthComponents object

    let applyEveryFilter = case optimization of
                            JPGOptimization        -> applyEveryFilterJPG
                            XRefStreamOptimization -> applyEveryFilterXRef
                            _anyOtherOptimization  -> applyEveryFilterGeneric

    candidates <- applyEveryFilter widthComponents stream <&> mkArray

    let
      original      = FilterCombination filters stream True
      bestCandidate = minimumBy resultCompare candidates

    -- Do nothing if the best result is worse than the original stream.
    if resultLoad bestCandidate < resultLoad original
      then
        if fcReplace bestCandidate
          then
            setStream (fcBytes bestCandidate) object
              >>= setFilters (fcList bestCandidate)
          else
            setStream (fcBytes bestCandidate) object
              >>= setFilters (fcList bestCandidate >< fcList original)
      else return object
  else return object
 where
  resultCompare :: FilterCombination -> FilterCombination -> Ordering
  resultCompare load1 load2 = compare (resultLoad load1) (resultLoad load2)

  resultLoad :: FilterCombination -> Int
  resultLoad filterCombination =
      fcLength filterCombination
    + foldr ((+) . filterLoad) 0 (fcList filterCombination)

  filterLoad :: Filter -> Int
  filterLoad (Filter f p) = BS.length (fromPDFObject f)
                          + BS.length (fromPDFObject p)
