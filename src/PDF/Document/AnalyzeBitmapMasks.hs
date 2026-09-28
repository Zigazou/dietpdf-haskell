-- | Detect bitmap mask roles and analyze supported streams without modifying
-- the document. Color-key arrays and graphics-state soft-mask groups are not
-- bitmap masks. Unsupported data remains detectable with an explicit reason.
-- Currently supports 1-bit stencils and 1/8-bit DeviceGray soft masks, with
-- default or reversed Decode, using lossless stream decoders and the strict
-- CCITT Group-4 subset emitted by this project. Call
-- after importObjects; results are returned to the caller, not cached in the
-- optimization state. Indirect geometry entries are unsupported.
module PDF.Document.AnalyzeBitmapMasks
  ( BitmapMaskRole (..), BitmapMaskInfo (..), analyzeBitmapMasks, readBitmapMask
  ) where

import Codec.Compression.CCITTG4 (decodeG4)

import Control.Monad.State (StateT)
import Control.Monad.Trans.State (gets)

import Data.Bitmap.MaskAnalysis (MaskAnalysis, MaskThresholds, analyzeMask)
import Data.Bits (testBit)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Fallible (FallibleT)
import Data.Foldable (toList)
import Data.IntMap.Strict (IntMap, Key)
import Data.IntMap.Strict qualified as IM
import Data.Kind (Type)
import Data.Logging (Logging)
import Data.PDF.Filter (Filter (Filter))
import Data.PDF.PDFObject
  ( PDFObject (PDFArray, PDFBool, PDFIndirectObjectWithStream, PDFName, PDFNumber, PDFReference)
  )
import Data.PDF.PDFPartition
  (PDFPartition (ppObjectsWithStream, ppObjectsWithoutStream))
import Data.PDF.PDFWork (PDFWork, tryP)
import Data.PDF.WorkData (WorkData (wPDF))
import Data.Set (Set)
import Data.Set qualified as Set

import PDF.Object.Container (getFilters)
import PDF.Object.Object.Properties (getValueForKey)
import PDF.Object.State (getStream)
import PDF.Processing.Unfilter (unfilter)

type BitmapMaskRole :: Type
-- | The relationship by which an image object is used as a mask. A single
-- image may have multiple roles when it is shared by other images.
data BitmapMaskRole
  -- | The image is referenced by an image's @SMask@ entry.
  = SoftMask
  -- | The image is referenced by an image's @Mask@ entry.
  | ExplicitMask
  -- | The image declares @ImageMask true@ and paints as a stencil.
  | StencilMask
  deriving stock (Eq, Ord, Show)

type BitmapMaskInfo :: Type
-- | Detection and analysis results for one indirect bitmap mask object.
data BitmapMaskInfo = BitmapMaskInfo
  -- | PDF object number of the mask.
  { bitmapMaskObject   :: !Int
  -- | Every detected use of this object; shared objects can have several.
  , bitmapMaskRoles    :: !(Set BitmapMaskRole)
  -- | Decoded alpha analysis, or a reason why the mask is unsupported.
  , bitmapMaskAnalysis :: !(Either String MaskAnalysis)
  } deriving stock (Eq, Show)

-- | Analyze masks in the imported PDF partition. Shared masks are analyzed
-- once, retaining all roles. This action does not update objects or work data.
analyzeBitmapMasks :: Logging m => MaskThresholds -> PDFWork m [BitmapMaskInfo]
analyzeBitmapMasks thresholds = do
  partition <- gets wPDF
  let
    objects :: IntMap PDFObject
    objects = IM.union (ppObjectsWithStream partition)
                       (ppObjectsWithoutStream partition)

    images :: IntMap PDFObject
    images = IM.filter isImage objects

    roles :: IntMap (Set BitmapMaskRole)
    roles = IM.foldlWithKey' (detect objects) IM.empty images

  mapM (inspect objects) (IM.toAscList roles)
 where
  -- Recognize image dictionaries by their subtype entry.
  isImage :: PDFObject -> Bool
  isImage object = getValueForKey "Subtype" object == Just (PDFName "Image")

  -- Add stencil status and valid image references found in mask entries.
  detect
    :: IntMap PDFObject
    -> IntMap (Set BitmapMaskRole)
    -> Int
    -> PDFObject
    -> IntMap (Set BitmapMaskRole)
  detect objects acc number object =
    let
      stencil :: Bool
      stencil = getValueForKey "ImageMask" object == Just (PDFBool True)

      acc' :: IntMap (Set BitmapMaskRole)
      acc' =
        if stencil
          then IM.insertWith Set.union number (Set.singleton StencilMask) acc
          else acc

      -- Record a mask reference only when its target and revision match.
      add
        :: BitmapMaskRole
        -> ByteString
        -> IntMap (Set BitmapMaskRole)
        -> IntMap (Set BitmapMaskRole)
      add role key found = case getValueForKey key object of
        Just (PDFReference target revision) -> case IM.lookup target objects of
          Just candidate@(PDFIndirectObjectWithStream _ actualRevision _ _)
            | revision == actualRevision && isImage candidate ->
              IM.insertWith Set.union target (Set.singleton role) found

          _ -> found

        _ -> found
    in
      add SoftMask "SMask" (add ExplicitMask "Mask" acc')

  -- Decode and analyze a detected object, preserving per-mask failures.
  inspect
    :: Logging m
    => IntMap PDFObject
    -> (Key, Set BitmapMaskRole)
    -> StateT WorkData (FallibleT m) BitmapMaskInfo
  inspect objects (number, roles) = do
    result <- case IM.lookup number objects of
      Just object -> do
        decoded <- readBitmapMask roles object
        return $ decoded >>= \(width, height, alpha) ->
          analyzeMask thresholds width height alpha

      Nothing -> return (Left "Missing bitmap mask object")

    return (BitmapMaskInfo number roles result)

-- | Decode supported masks to one effective alpha byte per pixel. Geometry,
-- polarity and stream-length checks are shared by analysis and optimization.
readBitmapMask
  :: Logging m
  => Set BitmapMaskRole
  -> PDFObject
  -> PDFWork m (Either String (Int, Int, ByteString))
readBitmapMask roles object = case geometry roles object of
  Left reason -> return (Left reason)

  Right (width, height, bits, invert) -> do
    decoded <- tryP $ do
      plain <- unfilter object
      filters <- getFilters plain
      raw <- getStream plain

      -- Accept the strict Group-4 subset emitted by our encoder. Other fax
      -- modes remain untouched instead of guessing at decoding parameters.
      return $ case toList filters of
        [Filter (PDFName "CCITTFaxDecode") parms]
          | bits == 1
          , getValueForKey "K" parms == Just (PDFNumber (-1))
          , getValueForKey "Columns" parms
              == Just (PDFNumber (fromIntegral width))
          , getValueForKey "Rows" parms
              == Just (PDFNumber (fromIntegral height))
          , getValueForKey "EndOfBlock" parms == Just (PDFBool False)
          , all (\key -> getValueForKey key parms `elem`
                          [Nothing, Just (PDFBool False)]
                )
                ["EndOfLine", "EncodedByteAlign"]
          , getValueForKey "BlackIs1" parms `elem`
              [Nothing, Just (PDFBool False), Just (PDFBool True)] ->
              case decodeG4 width height raw of
                Right packed -> (mempty,
                  if getValueForKey "BlackIs1" parms == Just (PDFBool True)
                    then packed
                    else BS.map (255 -) packed)

                Left _invalidFax -> (filters, raw)

        _other -> (filters, raw)

    return $ case decoded of
      Left err -> Left (show err)
      Right (filters, raw)
        | not (null filters) -> Left "Unsupported mask filter"
        | toInteger (BS.length raw)
            /= ((toInteger width * toInteger bits + 7) `div` 8)
               * toInteger height -> Left "Invalid mask stream length"
        | otherwise ->
            let
              stride :: Int
              stride = fromInteger ((toInteger width + 7) `div` 8)

              alpha :: ByteString
              alpha = if bits == 8 then raw else BS.pack
                [ if testBit (BS.index raw (y * stride + x `div` 8))
                              (7 - x `mod` 8) then 255 else 0
                | y <- [0 .. height - 1], x <- [0 .. width - 1]
                ]
            in
              Right (width
                    , height
                    , if invert then BS.map (255 -) alpha else alpha
                    )

-- Only exact default/reversed Decode ranges are supported. Stencil samples
-- use the opposite painting polarity to soft-mask alpha samples.
-- | Validate supported mask geometry and derive its sample depth and polarity.
-- The final flag says whether decoded samples must be inverted to become alpha.
geometry
  :: Set BitmapMaskRole
  -> PDFObject
  -> Either String (Int, Int, Int, Bool)
geometry roles object = do
  width <- dimension "Width"
  height <- dimension "Height"
  bits <- case getValueForKey "BitsPerComponent" object of
    Nothing | stencil                -> Right 1
    Just (PDFNumber 1)               -> Right 1
    Just (PDFNumber 8) | not stencil -> Right 8
    _                                -> Left "Unsupported mask bit depth"

  if Set.member SoftMask roles
      && (  stencil
         || getValueForKey "ColorSpace" object /= Just (PDFName "DeviceGray")
         )
    then Left "Unsupported soft-mask color space or image-mask flag"
    else Right ()

  if Set.member ExplicitMask roles && not stencil
    then Left "Explicit bitmap mask must declare ImageMask true"
    else Right ()

  reversed <- case getValueForKey "Decode" object of
    Nothing -> Right False
    Just (PDFArray (toList -> [PDFNumber 0, PDFNumber 1])) -> Right False
    Just (PDFArray (toList -> [PDFNumber 1, PDFNumber 0])) -> Right True
    _ -> Left "Unsupported mask Decode array"

  if toInteger width * toInteger height > toInteger (maxBound :: Int)
    then Left "Mask dimensions overflow"
    else Right (width, height, bits, stencil /= reversed)

 where
  -- Whether the image uses stencil painting semantics.
  stencil :: Bool
  stencil = getValueForKey "ImageMask" object == Just (PDFBool True)

  -- Read a positive, integral, in-range dimension from the image dictionary.
  dimension :: ByteString -> Either String Int
  dimension key =
    case getValueForKey key object of
      Just (PDFNumber value)
        | value > 0 && value <= fromIntegral (maxBound :: Int)
        , let integer = floor value :: Integer
        , fromInteger integer == value
        , integer <= toInteger (maxBound :: Int) -> Right (fromInteger integer)

      _ -> Left "Invalid mask dimensions"
