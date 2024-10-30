{-|
This modules implements several optimization techniques targeted at PDF objects.
-}
module PDF.Processing.Filter
  ( filterOptimize
  ) where

import Control.Monad.State (gets, lift)
import Control.Monad.Trans.Except (except)

import Data.Array (mkArray)
import Data.ByteString (ByteString)
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
  (OptimizationType (JPGOptimization, XRefStreamOptimization))
import Data.PDF.PDFObject
  ( PDFObject (PDFName, PDFNull, PDFNumber, PDFNumber, PDFXRefStream)
  , getObjectNumber
  , hasStream
  )
import Data.PDF.PDFWork (PDFWork, getMasks, sayComparisonP, withContext)
import Data.PDF.Settings (UseZopfli (UseDeflate, UseZopfli), sZopfli)
import Data.PDF.WorkData (wSettings)
import Data.Sequence ((><))
import Data.Set qualified as Set
import Data.Text qualified as T

import External.JpegToJpeg2k (jpegToJpeg2k)

import PDF.Document.XRef (xrefStreamWidth)
import PDF.Object.Container (getFilters, setFilters)
import PDF.Object.Object (fromPDFObject)
import PDF.Object.State (getStream, getValue, setStream)
import PDF.Processing.FilterCombine.Jpeg2k (jpeg2k)
import PDF.Processing.FilterCombine.PredRleZopfli (predRleZopfli)
import PDF.Processing.FilterCombine.PredZopfli (predZopfli)
import PDF.Processing.FilterCombine.Rle (rle)
import PDF.Processing.FilterCombine.RleZopfli (rleZopfli)
import PDF.Processing.FilterCombine.Zopfli (zopfli)

filterInfo
  :: Logging m
  => T.Text
  -> ByteString
  -> ByteString
  -> PDFWork m ()
filterInfo filterName streamBefore streamAfter =
  sayComparisonP ("filter " <> filterName)
                 (BS.length streamBefore)
                 (BS.length streamAfter)

applyEveryFilterGeneric
  :: Logging IO
  => Bool
  -> Maybe (Int, Int)
  -> ByteString
  -> PDFWork IO [FilterCombination]
-- The stream has width and components information.
applyEveryFilterGeneric objectIsAMask widthComponents@(Just (width, components)) stream = do
  useZopfli <- gets (sZopfli . wSettings)

  let rNothing = mkFCAppend [] stream
      jpeg2kParameters = Just ( width
                              , BS.length stream `div` (width * components)
                              , fromComponents components
                              )
      quality = if objectIsAMask then Just 50 else Nothing

  rJpeg2k <- jpeg2k quality jpeg2kParameters stream
  filterInfo "JPEG2000" stream (fcBytes rJpeg2k)

  rRle <- lift (except $ rle widthComponents stream)
  filterInfo "RLE" stream (fcBytes rRle)

  rZopfli <- lift (except $ zopfli widthComponents stream useZopfli)

  case useZopfli of
    UseZopfli  -> filterInfo "Zopfli" stream (fcBytes rZopfli)
    UseDeflate -> filterInfo "Deflate" stream (fcBytes rZopfli)

  rleCombine <- if fcLength rRle < BS.length stream
    then do
      rRleZopfli <- lift (except $ rleZopfli widthComponents stream useZopfli)
      case useZopfli of
        UseZopfli  -> filterInfo "RLE+Zopfli" stream (fcBytes rRleZopfli)
        UseDeflate -> filterInfo "RLE+Deflate" stream (fcBytes rRleZopfli)

      return [rRle, rRleZopfli]
    else
      return []

  rPredZopfli <- lift (except $ predZopfli widthComponents stream useZopfli)
  case useZopfli of
    UseZopfli  -> filterInfo "Predictor/Zopfli" stream (fcBytes rPredZopfli)
    UseDeflate -> filterInfo "Predictor/Deflate" stream (fcBytes rPredZopfli)

  rPredRleZopfli <- lift (except $ predRleZopfli widthComponents stream useZopfli)
  case useZopfli of
    UseZopfli  -> filterInfo "Predictor/RLE+Zopfli" stream (fcBytes rPredRleZopfli)
    UseDeflate -> filterInfo "Predictor/Store+RLE+Deflate" stream (fcBytes rPredRleZopfli)

  return $ [rNothing, rZopfli, rPredZopfli, rPredRleZopfli, rJpeg2k]
        ++ rleCombine

-- The stream has no width nor components information.
applyEveryFilterGeneric _objectIsAMask Nothing stream = do
  useZopfli <- gets (sZopfli . wSettings)

  let rNothing = mkFCAppend [] stream

  rRle <- lift (except $ rle Nothing stream)
  filterInfo "RLE" stream (fcBytes rRle)

  rZopfli <- lift (except $ zopfli Nothing stream useZopfli)
  case useZopfli of
    UseZopfli  -> filterInfo "Zopfli" stream (fcBytes rZopfli)
    UseDeflate -> filterInfo "Deflate" stream (fcBytes rZopfli)

  if fcLength rRle < BS.length stream
    then do
      rRleZopfli <- lift (except $ rleZopfli Nothing stream useZopfli)
      case useZopfli of
        UseZopfli  -> filterInfo "RLE+Zopfli" stream (fcBytes rRleZopfli)
        UseDeflate -> filterInfo "RLE+Deflate" stream (fcBytes rRleZopfli)

      return [rNothing, rRle, rZopfli, rRleZopfli]
    else
      return [rNothing, rZopfli]

applyEveryFilterJPG
  :: Logging IO
  => Bool
  -> Maybe (Int, Int)
  -> ByteString
  -> PDFWork IO [FilterCombination]
-- The stream has width and components information.
applyEveryFilterJPG _objectIsAMask (Just _imageProperty) stream = do
  useZopfli <- gets (sZopfli . wSettings)
  let rNothing = mkFCAppend [] stream

  rZopfli <- lift (except $ zopfli Nothing stream useZopfli)
  case useZopfli of
    UseZopfli  -> filterInfo "Zopfli" stream (fcBytes rZopfli)
    UseDeflate -> filterInfo "Deflate" stream (fcBytes rZopfli)

  -- Try Jpeg2000 for images with less than 4 components.
  let jpeg2kQuality :: Int
      jpeg2kQuality = 60

  rJpeg2k <- lift (jpegToJpeg2k jpeg2kQuality stream)
         <&> mkFCReplace [Filter (PDFName "JPXDecode") PDFNull]
  filterInfo "JPEG2000" stream (fcBytes rJpeg2k)

  return [rNothing, rZopfli, rJpeg2k]

applyEveryFilterJPG _objectIsAMask Nothing stream = do
  useZopfli <- gets (sZopfli . wSettings)
  let rNothing = mkFCAppend [] stream

  rZopfli <- lift (except $ zopfli Nothing stream useZopfli)
  case useZopfli of
    UseZopfli  -> filterInfo "Zopfli" stream (fcBytes rZopfli)
    UseDeflate -> filterInfo "Deflate" stream (fcBytes rZopfli)

  return [rNothing, rZopfli]

applyEveryFilterXRef
  :: Logging m
  => Maybe (Int, Int)
  -> ByteString
  -> PDFWork m [FilterCombination]
-- The stream has width and components information.
applyEveryFilterXRef widthComponents@(Just (_width, _components)) stream = do
  useZopfli <- gets (sZopfli . wSettings)
  rPredZopfli <- lift (except $ predZopfli widthComponents stream useZopfli)
  case useZopfli of
    UseZopfli  -> filterInfo "Predictor+Zopfli" stream (fcBytes rPredZopfli)
    UseDeflate -> filterInfo "Predictor+Deflate" stream (fcBytes rPredZopfli)
  return [rPredZopfli]
applyEveryFilterXRef Nothing stream = do
  useZopfli <- gets (sZopfli . wSettings)
  let rNothing = mkFCAppend [] stream

  rZopfli <- lift (except $ zopfli Nothing stream useZopfli)
  case useZopfli of
    UseZopfli  -> filterInfo "Zopfli" stream (fcBytes rZopfli)
    UseDeflate -> filterInfo "Deflate" stream (fcBytes rZopfli)
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
      masks           <- getMasks

      let objectIsAMask :: Bool
          objectIsAMask = case getObjectNumber object of
                            Just major    -> Set.member major masks
                            _anyOtherCase -> False

      -- Find appropriate filters for the stream.
      let applyEveryFilter =
            case optimization of
              JPGOptimization        -> applyEveryFilterJPG objectIsAMask
              XRefStreamOptimization -> applyEveryFilterXRef
              _anyOtherOptimization  -> applyEveryFilterGeneric objectIsAMask

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
