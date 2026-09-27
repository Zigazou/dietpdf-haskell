-- | Detect bitmap mask roles and analyze supported streams without modifying
-- the document. Color-key arrays and graphics-state soft-mask groups are not
-- bitmap masks. Unsupported data remains detectable with an explicit reason.
-- Currently supports 1-bit stencils and 1/8-bit DeviceGray soft masks, with
-- default or reversed Decode, using the existing lossless stream decoders. Call
-- after importObjects; results are returned to the caller, not cached in the
-- optimization state. Indirect geometry entries are unsupported.
module PDF.Document.AnalyzeBitmapMasks
  ( BitmapMaskRole (..), BitmapMaskInfo (..), analyzeBitmapMasks
  ) where

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
data BitmapMaskRole
  = SoftMask
  | ExplicitMask
  | StencilMask
  deriving stock (Eq, Ord, Show)

type BitmapMaskInfo :: Type
data BitmapMaskInfo = BitmapMaskInfo
  { bitmapMaskObject   :: !Int
  , bitmapMaskRoles    :: !(Set BitmapMaskRole)
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
  isImage :: PDFObject -> Bool
  isImage object = getValueForKey "Subtype" object == Just (PDFName "Image")

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

  inspect
    :: Logging m
    => IntMap PDFObject
    -> (Key, Set BitmapMaskRole)
    -> StateT WorkData (FallibleT m) BitmapMaskInfo
  inspect objects (number, roles) = do
    result <- case IM.lookup number objects of
      Just object -> case geometry roles object of
        Left reason -> return (Left reason)

        Right (width, height, bits, invert) -> do
          decoded <- tryP $ do
            plain <- unfilter object
            filters <- getFilters plain
            raw <- getStream plain
            return (filters, raw)

          return $ case decoded of
            Left err -> Left (show err)

            Right (filters, raw)
              | not (null filters) -> Left "Unsupported mask filter"

              | toInteger (BS.length raw)
                  /= ((toInteger width * toInteger bits + 7) `div` 8)
                     * toInteger height ->
                Left "Invalid mask stream length"

              | otherwise ->
                  let
                    alpha :: ByteString
                    alpha =
                      if bits == 8
                        then
                          raw
                        else
                          BS.pack
                            [ if testBit (BS.index raw (y * stride + x `div` 8))
                                         (7 - x `mod` 8)
                                then 255
                                else 0
                            | y <- [0 .. height - 1], x <- [0 .. width - 1]
                            ]

                    stride :: Int
                    stride = fromInteger ((toInteger width + 7) `div` 8)
                  in
                    analyzeMask thresholds
                                width
                                height
                                (if invert then BS.map (255 -) alpha else alpha)

      Nothing -> return (Left "Missing bitmap mask object")

    return (BitmapMaskInfo number roles result)

-- Only exact default/reversed Decode ranges are supported. Stencil samples
-- use the opposite painting polarity to soft-mask alpha samples.
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
  stencil :: Bool
  stencil = getValueForKey "ImageMask" object == Just (PDFBool True)

  dimension :: ByteString -> Either String Int
  dimension key =
    case getValueForKey key object of
      Just (PDFNumber value)
        | value > 0 && value <= fromIntegral (maxBound :: Int)
        , let integer = floor value :: Integer
        , fromInteger integer == value
        , integer <= toInteger (maxBound :: Int) -> Right (fromInteger integer)

      _ -> Left "Invalid mask dimensions"
