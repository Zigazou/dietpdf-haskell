-- | Page-scoped removal of invisible painting, before resource pruning.
module PDF.Document.InvisibleImages (removeInvisiblePageImages) where

import Control.Monad (guard, (>=>))
import Control.Monad.State (gets)

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (toList)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import Data.PDF.GFXObject (separateGfx)
import Data.PDF.PDFObject
  ( PDFObject (PDFArray, PDFDictionary, PDFIndirectObject, PDFIndirectObjectWithStream, PDFName, PDFNumber, PDFReference)
  )
import Data.PDF.PDFPartition (ppObjectsWithStream, ppObjectsWithoutStream)
import Data.PDF.PDFWork (PDFWork)
import Data.PDF.Program (Program, extractObjects, parseProgram)
import Data.PDF.Settings (OptimizeGFX (DoNotOptimizeGFX), sOptimizeGFX)
import Data.PDF.WorkData (wPDF, wSettings)

import PDF.Graphics.Geometry
  (Matrix, Rect (Rect), identity, intersection, matrixOf, transform)
import PDF.Graphics.InvisibleImages
  ( ImageInfo (ImageInfo)
  , VisibilityMode (GeometryOnly, IncludeOcclusion)
  , removeInvisibleImagesWithMode
  )
import PDF.Graphics.OutsidePage (FontInfo (FontInfo), removeOutsidePage)
import PDF.Graphics.Parser.Stream (gfxParse)
import PDF.Graphics.Visibility (finite)
import PDF.Object.Object.Properties (getValueForKey)

{- | The supplied stream must be a freshly merged, decoded page content stream.

It is never edited in place: callers allocate a new object for this page,
avoiding changes to streams shared with other pages, forms or appearances.
-}
removeInvisiblePageImages
  :: (Monad m)
  => PDFObject
  -> PDFObject
  -> PDFWork m PDFObject
removeInvisiblePageImages page content = do
  settings <- gets wSettings
  pdf <- gets wPDF
  let objects = ppObjectsWithoutStream pdf <> ppObjectsWithStream pdf
  return $ case sOptimizeGFX settings of
    DoNotOptimizeGFX -> content
    _ -> fromMaybe content $ do
      box <- pageViewport objects page
      optimizeContent (optimizePainting objects page box) content

{- | Follow references without expanding dictionaries or changing stream
objects. Both object number and generation must match; cycles are rejected.
-}
resolve :: IntMap PDFObject -> PDFObject -> Maybe PDFObject
resolve objects = go IS.empty
 where
  go seen (PDFReference number generation)
    | IS.member number seen = Nothing
    | otherwise = do
        object <- IM.lookup number objects
        case object of
          PDFIndirectObject n g inner
            | n == number && g == generation -> go (IS.insert number seen) inner
          PDFIndirectObjectWithStream n g _ _
            | n == number && g == generation -> Just object
          _ -> Nothing
  go seen (PDFIndirectObject _ _ inner) = go seen inner
  go _ object = Just object

{- | Resolve a dictionary value for the given key in a PDF object graph.
-}
value :: IntMap PDFObject -> ByteString -> PDFObject -> Maybe PDFObject
value objects key object = getValueForKey key object >>= resolve objects

{- | Look up an inherited dictionary key, following the parent chain when
needed.
-}
inherited :: IntMap PDFObject -> ByteString -> PDFObject -> Maybe PDFObject
inherited objects key = go IS.empty
 where
  go seen object = case getValueForKey key object of
    Just entry -> resolve objects entry
    Nothing -> case getValueForKey "Parent" object of
      Just reference@(PDFReference number _)
        | not (IS.member number seen) ->
            resolve objects reference >>= go (IS.insert number seen)
      _ -> Nothing

{- | Convert a PDF numeric object to a finite rational value.
-}
numeric :: PDFObject -> Maybe Rational
numeric (PDFNumber n) | finite n = Just (toRational n)
numeric _ = Nothing

{- | Decode a PDF array into a rectangle, rejecting degenerate or non-finite
values.
-}
rectangle :: PDFObject -> Maybe Rect
rectangle (PDFArray values) = do
  [a, b, c, d] <- traverse numeric (toList values)
  guard (a < c && b < d)
  return (Rect a b c d)
rectangle _ = Nothing

{- | Compute the effective page viewport as the intersection of the media box
and crop box.
-}
pageViewport :: IntMap PDFObject -> PDFObject -> Maybe Rect
pageViewport objects page = do
  media <- inherited objects "MediaBox" page >>= rectangle
  crop <- maybe (Just media) rectangle (inherited objects "CropBox" page)
  intersection media crop

{- | Extract the resource dictionary entries for a specific resource category.
-}
resourceEntries
  :: IntMap PDFObject
  -> PDFObject
  -> ByteString
  -> [(ByteString, PDFObject)]
resourceEntries objects page key =
  case inherited objects "Resources" page >>= value objects key of
    Just (PDFDictionary entries) -> Map.toList entries
    _                            -> []

{- | Decode a resource dictionary into a name-to-value map.

The resource names remain separate from the object-specific decoders so the
caller can choose the appropriate PDF metadata interpretation.
-}
resourceMap
  :: (PDFObject -> Maybe a)
  -> [(ByteString, PDFObject)]
  -> Map ByteString a
resourceMap decode = Map.fromList . mapMaybe decodeEntry
 where
  decodeEntry (name, entry) = (name,) <$> decode entry

{- | Decode image metadata needed for conservative visibility analysis.

Opaque, non-transparency images with a simple color space may participate in
cover proofs.
-}
imageInfo :: IntMap PDFObject -> PDFObject -> Maybe ImageInfo
imageInfo objects entry = do
  image@(PDFIndirectObjectWithStream _ _ dictionary _) <- resolve objects entry
  guard (Map.lookup "Subtype" dictionary == Just (PDFName "Image"))
  let
    keysToCheck :: [ByteString]
    keysToCheck =
      [ "Mask"
      , "SMask"
      , "SMaskInData"
      , "ImageMask"
      , "OC"
      , "Alternates"
      , "OPI"
      ]

    colorSpaces :: [Maybe PDFObject]
    colorSpaces =
      [ Just (PDFName "DeviceGray")
      , Just (PDFName "DeviceRGB")
      , Just (PDFName "DeviceCMYK")
      ]

    solid :: Bool
    solid = all (`Map.notMember` dictionary) keysToCheck
         && value objects "ColorSpace" image `elem` colorSpaces

  return (ImageInfo solid)

{- | Decode a simple PDF font object into the descriptor box and character-width
mapping needed for text reachability checks.
-}
fontInfo :: IntMap PDFObject -> PDFObject -> Maybe FontInfo
fontInfo objects entry = do
  object <- resolve objects entry
  subtype <- value objects "Subtype" object
  guard (subtype `elem` map PDFName ["Type1", "TrueType", "MMType1"])
  descriptor <- value objects "FontDescriptor" object
  fontBox <- value objects "FontBBox" descriptor >>= rectangle
  first <- value objects "FirstChar" object >>= numeric
  PDFArray entries <- value objects "Widths" object
  widths <- traverse (resolve objects >=> numeric) (toList entries)

  guard
    ( first >= 0
    && first <= 255
    && first == fromInteger (round first)
    && length widths <= 256 - round first
    )

  return (FontInfo fontBox (Map.fromList (zip [round first ..] widths)))

{- | Extract the affine matrix from a PDF XObject dictionary when present.

The default matrix is the identity if no explicit Matrix entry exists.
-}
objectMatrix :: IntMap PDFObject -> PDFObject -> Maybe Matrix
objectMatrix objects object = case getValueForKey "Matrix" object of
  Nothing -> Just identity
  Just entry -> do
    PDFArray entries <- resolve objects entry
    traverse (resolve objects >=> numeric) (toList entries) >>= matrixOf

{- | Compute the transformed rectangular bounds for an image or form XObject.
-}
objectBounds :: IntMap PDFObject -> PDFObject -> Maybe Rect
objectBounds objects entry = do
  object <- resolve objects entry
  subtype <- value objects "Subtype" object
  case subtype of
    PDFName "Image" -> Just (Rect 0 0 1 1)
    PDFName "Form" -> do
      box <- value objects "BBox" object >>= rectangle
      matrix <- objectMatrix objects object
      return (transform matrix box)
    _ -> Nothing

{- | Apply the page-scoped painting optimizations in sequence.

This removes content outside the page and then removes invisible image calls
using resource metadata and geometry constraints.
-}
optimizePainting :: IntMap PDFObject -> PDFObject -> Rect -> Program -> Program
optimizePainting objects page box =
  removeOutsidePage box fonts bounds
    . removeInvisibleImagesWithMode mode box images
 where
  xobjects :: [(ByteString, PDFObject)]
  xobjects = resourceEntries objects page "XObject"

  images :: Map ByteString ImageInfo
  images = resourceMap (imageInfo objects) xobjects

  bounds :: Map ByteString Rect
  bounds = resourceMap (objectBounds objects) xobjects

  fonts :: Map ByteString FontInfo
  fonts = resourceMap (fontInfo objects) (resourceEntries objects page "Font")

  mode :: VisibilityMode
  mode =
    if isNothing (getValueForKey "Group" page)
      then IncludeOcclusion
      else GeometryOnly

{- | Re-encode a decoded page content stream only when it changed.

The stream length is updated to match the re-encoded output, and unsupported
streams are left untouched.
-}
optimizeContent :: (Program -> Program) -> PDFObject -> Maybe PDFObject
optimizeContent
  optimize
  content@(PDFIndirectObjectWithStream major minor dictionary bytes) = do

  guard (Map.notMember "Filter" dictionary)

  tokens <- either (const Nothing) Just (gfxParse bytes)

  let
    program :: Program
    program = parseProgram tokens

    optimized :: Program
    optimized = optimize program

    output :: ByteString
    output = separateGfx (extractObjects optimized)

  return $
    if optimized == program
      then content
      else
        PDFIndirectObjectWithStream
          major
          minor
          (Map.insert "Length"
                      (PDFNumber (fromIntegral (BS.length output)))
                      dictionary
          )
          output
optimizeContent _ _ = Nothing
