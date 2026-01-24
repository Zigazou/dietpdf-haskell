{-|
Stream filter optimization for PDF objects.

This module evaluates multiple filter combinations (Zopfli/Deflate, RunLength,
Predictor variants, and JPEG2000) to find the best representation for object
streams, taking into account object properties like width, components, masks,
and special cases (JPEG images, XRef streams). It reports size changes via
logging and updates the object's stream and filter list accordingly.
-}
module PDF.Processing.Filter
  ( filterOptimize) where

import Data.Array (mkArray)
import Data.Bitmap.BitmapConfiguration
  ( BitmapConfiguration (BitmapConfiguration, bcBitsPerComponent, bcComponents, bcLineWidth)
  )
import Data.Bitmap.BitsPerComponent (BitsPerComponent (BC8Bits))
import Data.ByteString qualified as BS
import Data.Context (Contextual (ctx))
import Data.Foldable (minimumBy)
import Data.Functor ((<&>))
import Data.Logging (Logging)
import Data.PDF.Filter (Filter (Filter))
import Data.PDF.FilterCombination
  (FilterCombination (FilterCombination), fcBytes, fcLength, fcList, fcReplace)
import Data.PDF.OptimizationType
  ( OptimizationType (GfxOptimization, ICCOptimization, JPGOptimization, ObjectStreamOptimization, TTFOptimization, XMLOptimization, XRefStreamOptimization)
  )
import Data.PDF.PDFObject
  ( PDFObject (PDFName, PDFNumber, PDFNumber, PDFXRefStream)
  , getObjectNumber
  , hasStream
  , streamLength
  )
import Data.PDF.PDFWork (PDFWork, getMasks, withContext)
import Data.Sequence ((><))
import Data.Set qualified as Set

import PDF.Document.XRef (xrefStreamWidth)
import PDF.Object.Container (getFilters, setFilters)
import PDF.Object.Object (fromPDFObject)
import PDF.Object.State (getStream, getValue, getValueDefault, setStream)
import PDF.Processing.ApplyFilter.Generic (applyEveryFilterGeneric)
import PDF.Processing.ApplyFilter.ICC (applyEveryFilterICC)
import PDF.Processing.ApplyFilter.JPG (applyEveryFilterJPG)
import PDF.Processing.ApplyFilter.OnlyZopfli (applyEveryFilterOnlyZopfi)
import PDF.Processing.ApplyFilter.Text (applyEveryFilterText)
import PDF.Processing.ApplyFilter.XRef (applyEveryFilterXRef)

{-|
Infer stream width and components from object keys (`Width`, `ColorSpace`). For
`XRef` streams, width is derived by `xrefStreamWidth` and components default to
1.
-}
getBitmapConfiguration
  :: Logging m
  => PDFObject
  -> PDFWork m (Maybe BitmapConfiguration)
getBitmapConfiguration object@PDFXRefStream{} =
  xrefStreamWidth object <&> Just . \width ->
    BitmapConfiguration
      { bcLineWidth        = width
      , bcComponents       = 1
      , bcBitsPerComponent = BC8Bits
      }

getBitmapConfiguration object = do
  width <- getValue "Width" object
  colorSpace <- getValue "ColorSpace" object
  bitsPerComponent <- getValueDefault "BitsPerComponent" (PDFNumber 8) object
    >>= \case Just (PDFNumber value) -> return $ toEnum . round $ value
              _anyOtherValue -> return BC8Bits

  let components :: Int
      components = case colorSpace of
        Just (PDFName "DeviceRGB" ) -> 3
        Just (PDFName "DeviceCMYK") -> 4
        _anyOtherValue              -> 1

  case width of
    Just (PDFNumber width') ->
      let bitmapConfig = BitmapConfiguration
            { bcLineWidth        = round width'
            , bcComponents       = components
            , bcBitsPerComponent = bitsPerComponent
            }
      in return (Just bitmapConfig)
    _anyOtherValue -> return Nothing

{-|
Optimize a stream for a `PDFObject` based on the selected `OptimizationType` and
object metadata.

Steps:

1. Determine applicable filter candidate generator (generic/JPG/XRef).
2. Generate candidates and pick the best by stream length + filter overhead.
3. If the best candidate improves on the original, update stream bytes and
   filter list; otherwise leave the object unchanged.

This logs size comparisons and operates within `PDFWork` context.
-}
filterOptimize
  :: Logging IO
  => OptimizationType
  -> PDFObject
  -> PDFWork IO PDFObject
filterOptimize optimization object
  | not (hasStream object) = return object
  | streamLength object == Just 0 = return object
  | otherwise = withContext (ctx ("filterOptimize" :: String)) $ do
    stream          <- getStream object
    filters         <- getFilters object
    bitmapConfig    <- getBitmapConfiguration object
    masks           <- getMasks

    let
      objectIsAMask :: Bool
      objectIsAMask = case getObjectNumber object of
                        Just major    -> Set.member major masks
                        _anyOtherCase -> False
      original = FilterCombination filters stream True
      applyEveryFilter =
        case optimization of
          GfxOptimization          -> applyEveryFilterText
          XMLOptimization          -> applyEveryFilterText
          ObjectStreamOptimization -> applyEveryFilterText
          TTFOptimization          -> applyEveryFilterOnlyZopfi
          JPGOptimization          -> applyEveryFilterJPG objectIsAMask
          ICCOptimization          -> applyEveryFilterICC
          XRefStreamOptimization   -> applyEveryFilterXRef
          _anyOtherOptimization    -> applyEveryFilterGeneric objectIsAMask

    -- Apply every filter to the stream and return the best result.
    candidates <- applyEveryFilter bitmapConfig stream <&> mkArray

    let
      bestCandidate = minimumBy resultCompare candidates

    -- Do nothing if the best result is worse than the original stream.
    if resultLoad bestCandidate >= resultLoad original
      then return object
      else
        if fcReplace bestCandidate
          then setStream (fcBytes bestCandidate) object
            >>= setFilters (fcList bestCandidate)
          else setStream (fcBytes bestCandidate) object
            >>= setFilters (fcList bestCandidate >< fcList original)
 where
  resultCompare :: FilterCombination -> FilterCombination -> Ordering
  resultCompare load1 load2 = compare (resultLoad load1) (resultLoad load2)

  -- The total load of a filter combination: compressed size + filter overhead.
  resultLoad :: FilterCombination -> Int
  resultLoad filterCombination =
      fcLength filterCombination
    + foldr ((+) . filterLoad) 0 (fcList filterCombination)

  filterLoad :: Filter -> Int
  filterLoad (Filter f p) = BS.length (fromPDFObject f)
                          + BS.length (fromPDFObject p)
