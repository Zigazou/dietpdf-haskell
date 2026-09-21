-- | Page-scoped removal of invisible image invocations, before resource pruning.
module PDF.Document.InvisibleImages (removeInvisiblePageImages) where

import Control.Monad.State (gets)

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (toList)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet(IntSet)
import Data.IntSet qualified as IS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing, mapMaybe)
import Data.PDF.GFXObject (separateGfx)
import Data.PDF.PDFObject
  ( PDFObject (PDFArray, PDFDictionary, PDFIndirectObject, PDFIndirectObjectWithStream, PDFName, PDFNumber, PDFReference)
  )
import Data.PDF.PDFPartition (ppObjectsWithStream, ppObjectsWithoutStream)
import Data.PDF.PDFWork (PDFWork)
import Data.PDF.Program (extractObjects, parseProgram)
import Data.PDF.Settings (OptimizeGFX (DoNotOptimizeGFX), sOptimizeGFX)
import Data.PDF.WorkData (wPDF, wSettings)

import PDF.Graphics.InvisibleImages
  ( ImageInfo (ImageInfo)
  , Rect (Rect)
  , VisibilityMode (GeometryOnly, IncludeOcclusion)
  , removeInvisibleImagesWithMode
  )
import PDF.Graphics.Parser.Stream (gfxParse)
import PDF.Object.Object.Properties (getValueForKey)

{- | The supplied stream must be a freshly merged, decoded page content stream.
It is never edited in place: callers allocate a new object for this page,
avoiding changes to streams shared with other pages, forms or appearances.
-}
removeInvisiblePageImages :: Monad m => PDFObject -> PDFObject -> PDFWork m PDFObject
removeInvisiblePageImages page content = do
  settings <- gets wSettings
  pdf <- gets wPDF

  let
    objects :: IntMap PDFObject
    objects = ppObjectsWithoutStream pdf <> ppObjectsWithStream pdf

    resolve :: IntSet -> PDFObject -> Maybe PDFObject
    resolve seen (PDFReference number generation)
      | IS.member number seen = Nothing
      | otherwise = do
          object <- IM.lookup number objects
          case object of
            PDFIndirectObject n g inner | n == number && g == generation ->
              resolve (IS.insert number seen) inner
            PDFIndirectObjectWithStream n g _ _ | n == number && g == generation ->
              Just object
            _anyOtherObject -> Nothing
    resolve seen (PDFIndirectObject _ _ inner) = resolve seen inner
    resolve _ object = Just object

    value :: ByteString -> PDFObject -> Maybe PDFObject
    value key object = getValueForKey key object >>= resolve IS.empty

    inherited :: IntSet -> ByteString -> PDFObject -> Maybe PDFObject
    inherited seen key object = case getValueForKey key object of
      Just entry -> resolve IS.empty entry
      Nothing -> case getValueForKey "Parent" object of
        Just reference@(PDFReference number _) | not (IS.member number seen) ->
          resolve IS.empty reference >>= inherited (IS.insert number seen) key
        _ -> Nothing

    rectangle :: PDFObject -> Maybe Rect
    rectangle (PDFArray values) = case toList values of
      [PDFNumber a,PDFNumber b,PDFNumber c,PDFNumber d]
        | all (\x -> not (isNaN x || isInfinite x)) [a,b,c,d], a < c, b < d ->
          Just (Rect (toRational a) (toRational b) (toRational c) (toRational d))
      _ -> Nothing
    rectangle _ = Nothing

    viewport :: Maybe Rect
    viewport = do
      media@(Rect a b c d) <- inherited IS.empty "MediaBox" page >>= rectangle
      crop <- case inherited IS.empty "CropBox" page of
        Nothing    -> Just media
        Just entry -> rectangle entry
      let Rect e f g h = crop
      if max a e < min c g && max b f < min d h
        then Just (Rect (max a e) (max b f) (min c g) (min d h))
        else Nothing

    imageInfo :: (ByteString, PDFObject) -> Maybe (ByteString, ImageInfo)
    imageInfo (name, entry) = do
      image <- resolve IS.empty entry
      case image of
        PDFIndirectObjectWithStream _ _ dictionary _
          | Map.lookup "Subtype" dictionary == Just (PDFName "Image") ->
              let solid = all (`Map.notMember` dictionary)
                            [ "Mask"
                            , "SMask"
                            , "SMaskInData"
                            , "ImageMask"
                            , "OC"
                            , "Alternates"
                            , "OPI"
                            ]
                        && value "ColorSpace" image `elem`
                            map (Just . PDFName) [ "DeviceGray"
                                                  , "DeviceRGB"
                                                  , "DeviceCMYK"
                                                  ]
              in Just (name, ImageInfo solid)
        _anyOtherObject -> Nothing

    imageResources :: Maybe (Map ByteString ImageInfo)
    imageResources = do
      resources <- inherited IS.empty "Resources" page
      PDFDictionary xobjects <- value "XObject" resources
      return (Map.fromList (mapMaybe imageInfo (Map.toList xobjects)))

  return $ case (sOptimizeGFX settings, content, viewport, imageResources) of
    (DoNotOptimizeGFX, _, _, _) -> content
    (_, PDFIndirectObjectWithStream major minor dictionary bytes, Just box, Just images)
      | Map.notMember "Filter" dictionary -> case gfxParse bytes of
          Right tokens ->
            let program = parseProgram tokens
                mode = if isNothing (getValueForKey "Group" page)
                         then IncludeOcclusion else GeometryOnly
                optimized = removeInvisibleImagesWithMode mode
                                                          box
                                                          images
                                                          program
                output = separateGfx (extractObjects optimized)
            in if optimized == program then content else
                 PDFIndirectObjectWithStream major minor
                   (Map.insert "Length"
                               (PDFNumber (fromIntegral (BS.length output)))
                               dictionary
                   )
                   output
          Left _ -> content
    _ -> content
