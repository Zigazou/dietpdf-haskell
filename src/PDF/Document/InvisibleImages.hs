-- | Page-scoped removal of invisible painting, before resource pruning.
module PDF.Document.InvisibleImages (removeInvisiblePageImages) where

import Control.Monad ((>=>))
import Control.Monad.State (gets)

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (toList)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet (IntSet)
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
import Data.PDF.Program (Program, extractObjects, parseProgram)
import Data.PDF.Settings (OptimizeGFX (DoNotOptimizeGFX), sOptimizeGFX)
import Data.PDF.WorkData (wPDF, wSettings)

import PDF.Graphics.InvisibleImages
  ( ImageInfo (ImageInfo)
  , Rect (Rect)
  , VisibilityMode (GeometryOnly, IncludeOcclusion)
  , bounds
  , removeInvisibleImagesWithMode
  )
import PDF.Graphics.OutsidePage (FontInfo (FontInfo), removeOutsidePage)
import PDF.Graphics.Parser.Stream (gfxParse)
import PDF.Object.Object.Properties (getValueForKey)

{- | The supplied stream must be a freshly merged, decoded page content stream.
It is never edited in place: callers allocate a new object for this page,
avoiding changes to streams shared with other pages, forms or appearances.
-}
removeInvisiblePageImages
  :: Monad m
  => PDFObject
  -> PDFObject
  -> PDFWork m PDFObject
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
            PDFIndirectObject n g inner
              | n == number && g == generation ->
              resolve (IS.insert number seen) inner

            PDFIndirectObjectWithStream n g _ _
              | n == number && g == generation ->
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
        Just (Rect (toRational a)
                    (toRational b)
                    (toRational c)
                    (toRational d)
             )
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

    resourceEntries :: ByteString -> [(ByteString, PDFObject)]
    resourceEntries key =
      case inherited IS.empty "Resources" page >>= value key of
        Just (PDFDictionary entries) -> Map.toList entries
        _noDictionary                -> []

    imageResources :: Map ByteString ImageInfo
    imageResources =
      Map.fromList (mapMaybe imageInfo (resourceEntries "XObject"))

    numeric :: PDFObject -> Maybe Rational
    numeric (PDFNumber n) | not (isNaN n || isInfinite n) = Just (toRational n)
    numeric _anyOtherObject                               = Nothing

    fontInfo :: (ByteString, PDFObject) -> Maybe (ByteString, FontInfo)
    fontInfo (name, entry) = do
      object <- resolve IS.empty entry
      subtype <- value "Subtype" object

      if subtype == PDFName "Type1"
        || subtype == PDFName "TrueType"
        || subtype == PDFName "MMType1"
        then do
          descriptor <- value "FontDescriptor" object
          fontBox <- value "FontBBox" descriptor >>= rectangle
          first <- value "FirstChar" object >>= numeric
          PDFArray entries <- value "Widths" object
          widths <- traverse (resolve IS.empty >=> numeric) (toList entries)

          if first >= 0
            && first <= 255
            && first == fromInteger (round first)
            && length widths <= 256 - round first
            then
              Just ( name
                  , FontInfo fontBox (Map.fromList (zip [round first..] widths))
                  )
            else
              Nothing
        else
          Nothing

    objectBounds :: (ByteString, PDFObject) -> Maybe (ByteString, Rect)
    objectBounds (name, entry) = do
      object <- resolve IS.empty entry
      subtype <- value "Subtype" object

      case subtype of
        PDFName "Image" -> Just (name, Rect 0 0 1 1)
        PDFName "Form" -> do
          Rect a b c d <- value "BBox" object >>= rectangle
          matrix <- case getValueForKey "Matrix" object of
            Nothing -> Just (1, 0, 0, 1, 0, 0)

            Just entryMatrix -> do
              PDFArray entries <- resolve IS.empty entryMatrix
              ns <- traverse (resolve IS.empty >=> numeric) (toList entries)

              case ns of
                [u,v,w,x,y,z] -> Just (u,v,w,x,y,z)
                _             -> Nothing

          Just (name, bounds matrix a b (c-a) (d-b))

        _anyOtherSubtype -> Nothing

  return $ case (sOptimizeGFX settings, content, viewport) of
    (DoNotOptimizeGFX, _, _) -> content
    (_, PDFIndirectObjectWithStream major minor dictionary bytes, Just box)
      | Map.notMember "Filter" dictionary -> case gfxParse bytes of
          Right tokens ->
            let
              program :: Program
              program = parseProgram tokens

              mode :: VisibilityMode
              mode = if isNothing (getValueForKey "Group" page)
                        then IncludeOcclusion
                        else GeometryOnly

              imagesOptimized :: Program
              imagesOptimized = removeInvisibleImagesWithMode
                                  mode
                                  box
                                  imageResources
                                  program

              optimized :: Program
              optimized =
                removeOutsidePage
                  box
                  (Map.fromList (mapMaybe fontInfo
                                          (resourceEntries "Font")
                                )
                  )
                  (Map.fromList (mapMaybe objectBounds
                                          (resourceEntries "XObject")
                                )
                  )
                  imagesOptimized

              output :: ByteString
              output = separateGfx (extractObjects optimized)
            in
              if optimized == program
                then
                  content
                else
                 PDFIndirectObjectWithStream major minor
                   (Map.insert "Length"
                               (PDFNumber (fromIntegral (BS.length output)))
                               dictionary
                   )
                   output

          Left _anyError -> content
    _anyOtherCase -> content
