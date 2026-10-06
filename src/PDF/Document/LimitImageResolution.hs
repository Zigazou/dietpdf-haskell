-- | Downsample images using their largest physical placement in the document.
module PDF.Document.LimitImageResolution
  ( limitImageResolution
  , imagePixelLimits
  ) where

import Control.Monad (forM_, guard, (>=>))
import Control.Monad.State (StateT, gets)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Fallible (FallibleT)
import Data.Foldable (toList)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, mapMaybe)
import Data.PDF.PDFObject
  ( PDFObject (PDFArray, PDFDictionary, PDFIndirectObject, PDFIndirectObjectWithStream, PDFName, PDFNumber, PDFReference)
  , getObjectNumber
  )
import Data.PDF.PDFPartition (ppObjectsWithStream, ppObjectsWithoutStream)
import Data.PDF.PDFWork (PDFWork, putObject, sayP)
import Data.PDF.Program (parseProgram)
import Data.PDF.Settings (sLimitDPI)
import Data.PDF.WorkData (WorkData, wPDF, wSettings)

import PDF.Document.ResourceContext (inherited, resolve, value)
import PDF.Graphics.Geometry (Matrix, affine, identity, matrixOf)
import PDF.Graphics.ImageBounds (imageSize, xObjectMatrices)
import PDF.Graphics.Parser.Stream (gfxParse)
import PDF.Graphics.Visibility (finite)
import PDF.Object.Object.Properties (getValueForKey)
import PDF.Processing.ResizeImage (resizeImage)
import PDF.Processing.Unfilter (unfilter)

-- | Represents the maximum required dimensions (width and height in pixels)
-- and the associated color space for an image.
type ImageRequirement :: Type
type ImageRequirement = (Int, Int, Maybe PDFObject)

-- | A monad transformer stack used for traversing the PDF structure.
-- It combines 'MaybeT' for early termination, 'StateT' for maintaining 'WorkData',
-- and 'FallibleT' for error handling in 'IO'.
type PlacementWork :: Type -> Type
type PlacementWork = MaybeT (StateT WorkData (FallibleT IO))

-- | Run after resource/object pruning, before other image optimizations. Shared
-- images are changed once, using the maximum requirements of all uses. If any
-- page or invoked Form cannot be analyzed, retain the original images: a
-- missing placement could otherwise cause irreversible over-downsampling.
limitImageResolution :: PDFWork IO ()
limitImageResolution = do
  requested <- gets (sLimitDPI . wSettings)
  case requested of
    Just dpi | finite dpi && dpi > 0 -> do
      sayP "Limiting image resolution"
      limits <- imageRequirements dpi
      objects <- gets
                  (
                    (\pdf -> ppObjectsWithoutStream pdf
                          <> ppObjectsWithStream pdf
                    )
                  . wPDF
                  )
      case limits of
        Nothing
          -> sayP "Image placements are uncertain, retaining original \
                  \resolution"

        Just requirements
          -> forM_ (IM.toList requirements)
            $ \(number, (width, height, colorSpace)) ->
              case IM.lookup number objects of
                Just original@(PDFIndirectObjectWithStream n g dictionary bytes)
                  | Just resolvedColor <- colorSpace -> do
                      let
                        normalized :: PDFObject
                        normalized =
                          PDFIndirectObjectWithStream
                            n
                            g
                            (Map.insert "ColorSpace" resolvedColor dictionary)
                            bytes

                      resized <- resizeImage objects (width, height) normalized

                      -- PDFObject equality compares indirect identity, not
                      -- content. A successful resize always changes at least
                      -- one dimension.
                      let
                        changed :: Bool
                        changed = getValueForKey "Width" resized
                                    /= getValueForKey "Width" original
                               || getValueForKey "Height" resized
                                    /= getValueForKey "Height" original

                      putObject (if changed then resized else original)

                _ -> return ()

    _ -> return ()

-- | Returns a map of image object numbers to their calculated pixel
-- dimensions (width, height) based on the provided DPI.
imagePixelLimits :: Double -> PDFWork IO (Maybe (IntMap (Int, Int)))
imagePixelLimits dpi =
  fmap (fmap (IM.map (\(w, h, _) -> (w, h))))
       (imageRequirements dpi)

-- | Calculates the maximum required size for all images in the document,
-- accounting for DPI, page scaling (UserUnit), and various placements
-- (Pages, Forms, etc.). It excludes "protected" images (like masks).
imageRequirements :: Double -> PDFWork IO (Maybe (IntMap ImageRequirement))
imageRequirements dpi = do
  objects <-
    gets ( (\pdf -> ppObjectsWithoutStream pdf <> ppObjectsWithStream pdf)
         . wPDF
         )

  if not (finite dpi && dpi > 0)
    then
      return Nothing
    else
      runMaybeT $ do
        pages <- mapM (pageLimits objects) [page | page <- IM.elems objects,
          getValueForKey "Type" page == Just (PDFName "Page")]

        let
          combined :: IntMap ImageRequirement
          combined = IM.unionsWith largest pages

          protected :: IntSet
          protected = protectedImages objects

        return (IM.filterWithKey (\number _ -> IS.notMember number protected)
                                 combined
               )
 where
  -- Merges two image requirements by taking the maximum of both width and
  -- height. If color spaces differ, it clears the color space.
  largest :: ImageRequirement -> ImageRequirement -> ImageRequirement
  largest (w, h, color) (x, y, otherColor) =
    ( max w x
    , max h y
    , if color == otherColor then color else Nothing
    )

  -- Calculates the image requirements for a specific PDF Page.
  pageLimits
    :: IntMap PDFObject
    -> PDFObject
    -> PlacementWork (IntMap ImageRequirement)
  pageLimits objects page = do
    media <- require (inherited objects "MediaBox" page >>= rectangle objects)
    crop <- require $ case inherited objects "CropBox" page of
      Nothing    -> Just media
      Just entry -> rectangle objects entry

    let
      a :: Rational
      b :: Rational
      c :: Rational
      d :: Rational
      e :: Rational
      f :: Rational
      g :: Rational
      h :: Rational
      (a, b, c, d) = media
      (e, f, g, h) = crop

    -- Ensure the crop box is valid and within the media box
    guard (max a e < min c g && max b f < min d h)

    unit <- require $ case getValueForKey "UserUnit" page of
      Nothing    -> Just 1
      Just entry -> resolve objects entry >>= positive

    resources <- require $ case inherited objects "Resources" page of
      Nothing -> Just (PDFDictionary Map.empty)
      found   -> found

    case getValueForKey "Contents" page of
      Nothing -> return IM.empty
      Just entry -> do
        bytes <- contentBytes objects IS.empty entry
        placements objects IS.empty 0 unit resources identity bytes

  -- Recursively extracts raw ByteString content from a PDF object, handling
  -- references and arrays, but skipping filtered streams.
  contentBytes
    :: IntMap PDFObject
    -> IntSet
    -> PDFObject
    -> PlacementWork ByteString
  contentBytes objects seen entry = case entry of
    PDFReference number _ -> do
      guard (IS.notMember number seen)
      object <- require (resolve objects entry)
      contentBytes objects (IS.insert number seen) object

    PDFArray entries ->
      BS.intercalate " " <$> mapM (contentBytes objects seen) (toList entries)

    object@PDFIndirectObjectWithStream{} -> do
      decoded <- lift (unfilter object)

      case decoded of
        PDFIndirectObjectWithStream _ _ dictionary bytes -> do
          guard (Map.notMember "Filter" dictionary)
          return bytes

        _ -> MaybeT (return Nothing)

    PDFIndirectObject _ _ inner -> contentBytes objects seen inner
    _                           -> MaybeT (return Nothing)

  -- Parses a content stream to find XObject references and then processes their
  -- placements.
  placements
    :: IntMap PDFObject
    -> IntSet
    -> Int
    -> Double
    -> PDFObject
    -> Matrix
    -> ByteString
    -> PlacementWork (IntMap ImageRequirement)
  placements objects seen depth unit resources initial bytes = do
    -- Limit recursion depth to prevent infinite loops in malformed PDFs
    guard (depth <= (64 :: Int))

    tokens <- require (either (const Nothing) Just (gfxParse bytes))
    calls <- require (xObjectMatrices initial (parseProgram tokens))
    results <- mapM (placement objects seen depth unit resources) calls

    return (IM.unionsWith largest results)

  -- Processes a single XObject reference. If it's an Image, it calculates
  -- dimensions; if it's a Form, it recurses into the form's contents.
  placement
    :: IntMap PDFObject
    -> IntSet
    -> Int
    -> Double
    -> PDFObject
    -> (ByteString, Matrix)
    -> PlacementWork (IntMap ImageRequirement)
  placement objects seen depth unit resources (name, matrix) = do
    xobjects <- require (value objects "XObject" resources)
    object <- require (value objects name xobjects)

    case value objects "Subtype" object of
      Just (PDFName "Image") -> do
        number <- require (getObjectNumber object)

        let
          w :: Double
          h :: Double
          (w, h) = imageSize matrix

        dimensions <- require ((,) <$> pixels (w * unit) <*> pixels (h * unit))

        let
          color :: Maybe PDFObject
          color = do
              entry <- value objects "ColorSpace" object
              case entry of
                PDFName alias | alias `notElem` [ "DeviceGray"
                                                , "DeviceRGB"
                                                , "DeviceCMYK"
                                                ] ->
                  value objects "ColorSpace" resources
                    >>= value objects alias

                _ -> Just entry

          width :: Int
          height :: Int
          (width, height) = dimensions

        return (IM.singleton number (width, height, color))

      Just (PDFName "Form") -> do
        number <- require (getObjectNumber object)

        guard (IS.notMember number seen)

        formMatrix <- require $ case getValueForKey "Matrix" object of
          Nothing -> Just identity

          Just entry -> do
            PDFArray entries <- resolve objects entry
            traverse (resolve objects >=> numeric) (toList entries) >>= matrixOf

        own <- require $ case getValueForKey "Resources" object of
          Nothing    -> Just resources
          Just entry -> resolve objects entry

        formBytes <- contentBytes objects IS.empty object

        placements objects
                   (IS.insert number seen)
                   (depth + 1)
                   unit
                   own
                   (affine matrix formMatrix)
                   formBytes

      _ -> MaybeT (return Nothing)

  -- Converts a physical dimension (in points) to a pixel count based on the
  -- current DPI.
  pixels :: Double -> Maybe Int
  pixels extent = do
    let
      count :: Double
      count = extent * dpi / 72

    guard (finite count && count >= 0 && count < fromIntegral (maxBound :: Int))

    -- At least one pixel is required for degenerate/smaller-than-one-pixel
    -- uses.
    return (max 1 (floor count))

-- | Lifts a 'Maybe' value into the 'MaybeT' transformer.
require :: Monad m => Maybe a -> MaybeT m a
require = MaybeT . return

-- | Attempts to convert a 'PDFObject' to a 'Rational'.
numeric :: PDFObject -> Maybe Rational
numeric (PDFNumber n) | finite n = Just (toRational n)
numeric _ = Nothing

-- | Attempts to convert a 'PDFObject' to a positive 'Double'.
positive :: PDFObject -> Maybe Double
positive (PDFNumber n) | finite n && n > 0 = Just n
positive _ = Nothing

-- | Extracts the four coordinates of a rectangle from a 'PDFObject' (array).
rectangle
  :: IntMap PDFObject
  -> PDFObject
  -> Maybe (Rational, Rational, Rational, Rational)
rectangle objects entry = do
  PDFArray entries <- resolve objects entry
  [a, b, c, d] <- traverse (resolve objects >=> numeric) (toList entries)
  guard (a < c && b < d)
  return (a, b, c, d)

-- | Identifies a set of image object numbers that should not be downsampled
-- because they are used in contexts like masks, patterns, or specific font
-- types (Type 3), where resizing would break the PDF structure.
protectedImages :: IntMap PDFObject -> IntSet
protectedImages objects = IS.unions (map (reachable IS.empty) roots)
 where
  -- Identifies root objects that might lead to protected images.
  roots :: [PDFObject]
  roots = concatMap special (IM.elems objects)

  -- Checks if an object is a special type (Mask, SMask, etc.) or uses
  -- a specific pattern/font type.
  special :: PDFObject -> [PDFObject]
  special object =
    mapMaybe (`getValueForKey` object) ["Mask", "SMask", "Alternates", "AP"]
      ++ [ object
         | getValueForKey "Type" object == Just (PDFName "Pattern")
         || isJust (getValueForKey "PatternType" object)
         || getValueForKey "Subtype" object == Just (PDFName "Type3")
         ]

  -- Recursively traverses a 'PDFReference' to find its actual object.
  reachable :: IntSet -> PDFObject -> IntSet
  reachable seen (PDFReference number _)
    | IS.member number seen
    = IS.empty

    | otherwise
    = maybe IS.empty
            (reachable (IS.insert number seen))
            (IM.lookup number objects)

  -- Recursively traverses indirect objects.
  reachable seen (PDFIndirectObject _ _ inner)
    = reachable seen inner

  -- Recursively traverses dictionary entries to find all reachable image
  -- objects.
  reachable seen object@(PDFIndirectObjectWithStream number _ dictionary _)
    | getValueForKey "Subtype" object == Just (PDFName "Image")
    = IS.singleton number

    | otherwise
    = IS.unions (map (reachable seen) (Map.elems dictionary))

  -- Recursively traverses dictionaries.
  reachable seen (PDFDictionary entries)
    = IS.unions (map (reachable seen) (Map.elems entries))

  -- Recursively traverses arrays.
  reachable seen (PDFArray entries)
    = IS.unions (map (reachable seen) (toList entries))

  -- Base case for recursion.
  reachable _ _
    = IS.empty
