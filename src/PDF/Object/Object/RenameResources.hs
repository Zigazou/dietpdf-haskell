module PDF.Object.Object.RenameResources
  ( renameResources
  , containsResources) where


import Data.ByteString (ByteString)
import Data.Functor ((<&>))
import Data.Logging (Logging)
import Data.Map qualified as Map
import Data.PDF.PDFDocument (PDFDocument, deepFind, toList)
import Data.PDF.PDFObject
  ( PDFObject (PDFDictionary, PDFIndirectObject, PDFIndirectObjectWithStream, PDFReference)
  )
import Data.PDF.PDFWork (PDFWork, getReference)
import Data.PDF.Resource
  ( Resource (ResColorSpace, ResExtGState, ResFont, ResPattern, ResProcSet, ResProperties, ResShading, ResXObject)
  , resName
  )
import Data.Set (Set)
import Data.Set qualified as Set
import Data.TranslationTable (TranslationTable, convert)

import PDF.Object.Object.Properties (getValueForKey, hasKey)

import Util.Dictionary (Dictionary)

getMajor :: PDFObject -> Set Int
getMajor (PDFReference major _) = Set.singleton major
getMajor (PDFIndirectObject major _minor _object) = Set.singleton major
getMajor (PDFIndirectObjectWithStream major _minor _dict _stream) =
  Set.singleton major
getMajor _anyOtherObject = mempty

convertResource
  :: TranslationTable Resource
  -> (ByteString -> Resource)
  -> ByteString
  -> ByteString
convertResource table constructor = resName . convert table . constructor

renameResource
  :: TranslationTable Resource
  -> ByteString
  -> (ByteString, PDFObject)
  -> (ByteString, PDFObject)
renameResource table "ColorSpace" (name, object) =
  (convertResource table ResColorSpace name, object)
renameResource table "Font" (name, object) =
  (convertResource table ResFont name, object)
renameResource table "XObject" (name, object) =
  (convertResource table ResXObject name, object)
renameResource table "ExtGState" (name, object) =
  (convertResource table ResExtGState name, object)
renameResource table "Properties" (name, object) =
  (convertResource table ResProperties name, object)
renameResource table "Pattern" (name, object) =
  (convertResource table ResPattern name, object)
renameResource table "Shading" (name, object) =
  (convertResource table ResShading name, object)
renameResource table "ProcSet" (name, object) =
  (convertResource table ResProcSet name, object)
renameResource _table _unknownResourceType (name, object) = (name, object)

isAResourceType :: ByteString -> Bool
isAResourceType "ColorSpace"  = True
isAResourceType "Font"        = True
isAResourceType "XObject"     = True
isAResourceType "ExtGState"   = True
isAResourceType "Properties"  = True
isAResourceType "Pattern"     = True
isAResourceType "Shading"     = True
isAResourceType "ProcSet"     = True
isAResourceType _anyOtherType = False

renameByResourceType
  :: TranslationTable Resource
  -> (ByteString, PDFObject)
  -> (ByteString, PDFObject)
renameByResourceType table (resourceType, PDFDictionary dict)
  | isAResourceType resourceType =
      ( resourceType
      , PDFDictionary ( Map.fromList
                      . fmap (renameResource table resourceType)
                      . Map.toList
                      $ dict
                      )
      )
  | otherwise =
      ( convertResource table ResColorSpace
      $ convertResource table ResColorSpace
      $ convertResource table ResFont
      $ convertResource table ResXObject
      $ convertResource table ResExtGState
      $ convertResource table ResProperties
      $ convertResource table ResPattern
      $ convertResource table ResShading
      $ convertResource table ResProcSet resourceType
      , PDFDictionary dict
      )
renameByResourceType table (resourceType, object) =
  ( convertResource table ResColorSpace
  $ convertResource table ResColorSpace
  $ convertResource table ResFont
  $ convertResource table ResXObject
  $ convertResource table ResExtGState
  $ convertResource table ResProperties
  $ convertResource table ResPattern
  $ convertResource table ResShading
  $ convertResource table ResProcSet resourceType
  , object
  )

renameResourcesInDictionary
  :: TranslationTable Resource
  -> Dictionary PDFObject
  -> Dictionary PDFObject
renameResourcesInDictionary table object =
  case Map.lookup "Resources" object of
    Just (PDFDictionary resources) ->
      Map.insert
        "Resources"
        (PDFDictionary $ renameResourcesInDictionary table resources)
        object
    _anyOtherCase ->
        Map.fromList
      . fmap (renameByResourceType table)
      . Map.toList
      $ object

renameResources
  :: TranslationTable Resource
  -> Set Int
  -> PDFObject
  -> PDFObject
renameResources table _containingResources object@(PDFDictionary dictionary)
  | hasKey "Resources" object =
    PDFDictionary (renameResourcesInDictionary table dictionary)
  | otherwise = object
renameResources table containingResources object
  | Set.intersection (getMajor object) containingResources /= mempty =
      case object of
        PDFIndirectObject number revision (PDFDictionary dict) ->
          PDFIndirectObject
            number
            revision
            (PDFDictionary (renameResourcesInDictionary table dict))
        PDFIndirectObjectWithStream number revision dict stream ->
          PDFIndirectObjectWithStream
            number
            revision
            (renameResourcesInDictionary table dict)
            stream
        _anyOtherObject -> object
  | otherwise = object

containsResources :: Logging m => PDFDocument -> PDFWork m (Set Int)
containsResources document = do
  let resourceEntries = toList (deepFind (hasKey "Resources") document)
  mapM getResourceReferences resourceEntries <&> Set.unions
 where
  getResourceReferences :: Logging m => PDFObject -> PDFWork m (Set Int)
  getResourceReferences object = case getValueForKey "Resources" object of
    Just dictionary@PDFDictionary{} -> do
      subResources <- getSubResourceReferences dictionary
      return $ getMajor object <> subResources

    Just reference@PDFReference{} -> do
      referencedObject <- getReference reference
      subResources <- getSubResourceReferences referencedObject
      return $ getMajor object <> getMajor reference <> subResources

    Just _anyObject -> return (getMajor object)

    _anyOtherObject -> return mempty

  getCategoryReferences :: ByteString -> PDFObject -> Set Int
  getCategoryReferences category object =
    case getValueForKey category object of
      Just categoryObject -> getMajor categoryObject
      _anyOtherObject     -> mempty

  getSubResourceReferences :: Logging m => PDFObject -> PDFWork m (Set Int)
  getSubResourceReferences object = do
    let colorSpace = getCategoryReferences "ColorSpace" object
        font       = getCategoryReferences "Font"       object
        xObject    = getCategoryReferences "XObject"    object
        extGState  = getCategoryReferences "ExtGState"  object
        properties = getCategoryReferences "Properties" object
        pattern_   = getCategoryReferences "Pattern"    object
        shading    = getCategoryReferences "Shading"    object
        procSet    = getCategoryReferences "ProcSet"    object

    return ( colorSpace
          <> font
          <> xObject
          <> extGState
          <> properties
          <> pattern_
          <> shading
          <> procSet
          <> getMajor object
          )
