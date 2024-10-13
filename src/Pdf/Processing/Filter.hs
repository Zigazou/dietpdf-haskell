{-|
This modules implements several optimization techniques targeted at PDF objects.
-}
module Pdf.Processing.Filter
  ( filterOptimize
  ) where

import Control.Monad.State (lift)
import Control.Monad.Trans.Except (except)

import Data.Array (mkArray)
import Data.ByteString qualified as BS
import Data.ColorSpace (fromComponents)
import Data.Context (Contextual (ctx))
import Data.Foldable (minimumBy)
import Data.Functor ((<&>))
import Data.Logging (Logging)
import Data.PDF.Filter (Filter (Filter))
import Data.PDF.FilterCombination
    ( FilterCombination (FilterCombination)
    , fcBytes
    , fcLength
    , fcList
    , fcReplace
    , mkFCAppend
    , mkFCReplace
    )
import Data.PDF.OptimizationType
    ( OptimizationType (JPGOptimization, XRefStreamOptimization)
    )
import Data.PDF.PDFObject
    ( PDFObject (PDFName, PDFNull, PDFNumber, PDFNumber, PDFXRefStream)
    )
import Data.PDF.PDFWork (PDFWork, sayComparisonP, withContext)
import Data.Sequence ((><))
import Data.Text qualified as T

import External.JpegToJpeg2k (jpegToJpeg2k)

import Pdf.Document.XRef (xrefStreamWidth)
import Pdf.Object.Container (getFilters, setFilters)
import Pdf.Object.Object (fromPDFObject)
import Pdf.Object.Object.Properties (hasStream)
import Pdf.Object.State (getStream, getValue, setStream)
import Pdf.Processing.FilterCombine.Jpeg2k (jpeg2k)
import Pdf.Processing.FilterCombine.PredRleZopfli (predRleZopfli)
import Pdf.Processing.FilterCombine.PredZopfli (predZopfli)
import Pdf.Processing.FilterCombine.Rle (rle)
import Pdf.Processing.FilterCombine.RleZopfli (rleZopfli)
import Pdf.Processing.FilterCombine.Zopfli (zopfli)

filterInfo
  :: Logging m
  => T.Text
  -> BS.ByteString
  -> BS.ByteString
  -> PDFWork m ()
filterInfo filterName streamBefore streamAfter =
  sayComparisonP ("filter " <> filterName)
                 (BS.length streamBefore)
                 (BS.length streamAfter)

applyEveryFilterGeneric
  :: Logging IO
  => Maybe (Int, Int)
  -> BS.ByteString
  -> PDFWork IO [FilterCombination]
-- The stream has width and components information.
applyEveryFilterGeneric widthComponents@(Just (width, components)) stream = do
  let rNothing = mkFCAppend [] stream
      jpeg2kParameters =
        Just ( width
             , BS.length stream `div` (width * components)
             , fromComponents components
             )

  rJpeg2k <- jpeg2k jpeg2kParameters stream
  filterInfo "JPEG2000" stream (fcBytes rJpeg2k)

  rRle <- lift (except $ rle widthComponents stream)
  filterInfo "RLE" stream (fcBytes rRle)

  rZopfli <- lift (except $ zopfli widthComponents stream)
  filterInfo "Zopfli" stream (fcBytes rZopfli)

  rleCombine <- if fcLength rRle < BS.length stream
    then do
      rRleZopfli <- lift (except $ rleZopfli widthComponents stream)
      filterInfo "RLE+Zopfli" stream (fcBytes rRleZopfli)

      return [rRle, rRleZopfli]
    else
      return []

  rPredZopfli <- lift (except $ predZopfli widthComponents stream)
  filterInfo "Predictor/Zopfli" stream (fcBytes rPredZopfli)

  rPredRleZopfli <- lift (except $ predRleZopfli widthComponents stream)
  filterInfo "Predictor/Store+RLE+Zopfli" stream (fcBytes rPredRleZopfli)

  return $ [rNothing, rZopfli, rPredZopfli, rPredRleZopfli, rJpeg2k]
        ++ rleCombine

-- The stream has no width nor components information.
applyEveryFilterGeneric Nothing stream = do
  let rNothing = mkFCAppend [] stream

  rRle <- lift (except $ rle Nothing stream)
  filterInfo "RLE" stream (fcBytes rRle)

  rZopfli <- lift (except $ zopfli Nothing stream)
  filterInfo "Zopfli" stream (fcBytes rZopfli)

  if fcLength rRle < BS.length stream
    then do
      rRleZopfli <- lift (except $ rleZopfli Nothing stream)
      filterInfo "RLE+Zopfli" stream (fcBytes rRleZopfli)

      return [rNothing, rRle, rZopfli, rRleZopfli]
    else
      return [rNothing, rZopfli]

applyEveryFilterJPG
  :: Logging IO
  => Maybe (Int, Int)
  -> BS.ByteString
  -> PDFWork IO [FilterCombination]
-- The stream has width and components information.
applyEveryFilterJPG (Just _imageProperty) stream = do
  let rNothing = mkFCAppend [] stream

  rZopfli <- lift (except $ zopfli Nothing stream)
  filterInfo "Zopfli" stream (fcBytes rZopfli)

  -- Try Jpeg2000 for images with less than 4 components.
  let jpeg2kQuality = 40 :: Int

  rJpeg2k <- lift (jpegToJpeg2k jpeg2kQuality stream)
         <&> mkFCReplace [Filter (PDFName "JPXDecode") PDFNull]
  filterInfo "JPEG2000" stream (fcBytes rJpeg2k)

  return [rNothing, rZopfli, rJpeg2k]

applyEveryFilterJPG Nothing stream = do
  let rNothing = mkFCAppend [] stream

  rZopfli <- lift (except $ zopfli Nothing stream)
  filterInfo "Zopfli" stream (fcBytes rZopfli)

  return [rNothing, rZopfli]

applyEveryFilterXRef
  :: Logging m
  => Maybe (Int, Int)
  -> BS.ByteString
  -> PDFWork m [FilterCombination]
-- The stream has width and components information.
applyEveryFilterXRef widthComponents@(Just (_width, _components)) stream = do
  rPredZopfli <- lift (except $ predZopfli widthComponents stream)
  filterInfo "Predictor+Zopfli" stream (fcBytes rPredZopfli)
  return [rPredZopfli]
applyEveryFilterXRef Nothing stream = do
  let rNothing = mkFCAppend [] stream

  rZopfli <- lift (except $ zopfli Nothing stream)
  filterInfo "Zopfli" stream (fcBytes rZopfli)
  return [rNothing, rZopfli]

getWidthComponents :: Logging m => PDFObject -> PDFWork m (Maybe (Int, Int))
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
  -> PDFWork IO PDFObject
filterOptimize optimization object =
  withContext (ctx ("filterOptimize" :: String) <> ctx object) $
    if hasStream object
    then do
      stream          <- getStream object
      filters         <- getFilters object
      widthComponents <- getWidthComponents object

      -- Find appropriate filters for the stream.
      let applyEveryFilter = case optimization of
                              JPGOptimization        -> applyEveryFilterJPG
                              XRefStreamOptimization -> applyEveryFilterXRef
                              _anyOtherOptimization  -> applyEveryFilterGeneric

      -- Apply every filter to the stream and return the best result.
      candidates <- applyEveryFilter widthComponents stream <&> mkArray

      let
        original      = FilterCombination filters stream True
        bestCandidate = minimumBy resultCompare candidates

      -- Do nothing if the best result is worse than the original stream.
      if resultLoad bestCandidate < resultLoad original
        then
          if fcReplace bestCandidate
            then setStream (fcBytes bestCandidate) object
             >>= setFilters (fcList bestCandidate)
            else setStream (fcBytes bestCandidate) object
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
