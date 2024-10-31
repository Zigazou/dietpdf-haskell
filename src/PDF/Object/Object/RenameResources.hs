module PDF.Object.Object.RenameResources
  ( renameResources
  , containsResources) where


import Data.ByteString (ByteString)
import Data.Map qualified as Map
import Data.PDF.PDFDocument (PDFDocument, deepFind)
import Data.PDF.PDFObject
  ( PDFObject (PDFDictionary, PDFIndirectObject, PDFIndirectObjectWithStream, PDFReference)
  , isReference
  )
import Data.PDF.Resource
  ( Resource (ResColorSpace, ResExtGState, ResFont, ResPattern, ResProcSet, ResProperties, ResShading, ResXObject)
  , resName
  )
import Data.Set (Set)
import Data.Set qualified as Set
import Data.TranslationTable (TranslationTable, convert)

import PDF.Object.Object.Properties (getValueForKey, setValueForKey)

import Util.Dictionary (Dictionary)

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
renameResource _table _unknownResource (name, object) = (name, object)

renameByResourceType
  :: TranslationTable Resource
  -> (ByteString, PDFObject)
  -> (ByteString, PDFObject)
renameByResourceType table (resourceType, PDFDictionary dict) =
  ( resourceType
  , PDFDictionary ( Map.fromList
                  . fmap (renameResource table resourceType)
                  . Map.toList
                  $ dict
                  )
  )
renameByResourceType _table (resourceType, object) = (resourceType, object)

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
renameResources table _containingResources dictionary@PDFDictionary{} =
  case getValueForKey "Resources" dictionary of
    Just (PDFDictionary resources) -> setValueForKey
        "Resources"
        (Just . PDFDictionary $ renameResourcesInDictionary table resources)
        dictionary
    _anyOtherCase -> dictionary
renameResources table containingResources object@(PDFIndirectObject number revision (PDFDictionary dict))
  | Set.member number containingResources =
      PDFIndirectObject
        number
        revision
        (PDFDictionary (renameResourcesInDictionary table dict))
  | otherwise = object
renameResources table containingResources object@(PDFIndirectObjectWithStream number revision dict stream)
  | Set.member number containingResources =
      PDFIndirectObjectWithStream
        number
        revision
        (renameResourcesInDictionary table dict)
        stream
  | otherwise = object
renameResources _table _containingResources object = object

containsResources :: PDFDocument -> Set Int
containsResources = foldMap getResourceReferences . deepFind hasResourceEntry
  where
    hasResourceEntry :: PDFObject -> Bool
    hasResourceEntry object = case getValueForKey "Resources" object of
      Just _  -> True
      Nothing -> False

    getResourceReferences :: PDFObject -> Set Int
    getResourceReferences object = case getValueForKey "Resources" object of
      Just (PDFDictionary dict) ->
        Set.singleton (getMajor object)
          <> ( Set.fromList
             . fmap getMajor
             . Map.elems
             . Map.filter isReference
             ) dict

      Just reference@PDFReference{} -> Set.singleton (getMajor reference)
      _anyOtherObject               -> mempty

    getMajor :: PDFObject -> Int
    getMajor (PDFReference major _)                                   = major
    getMajor (PDFIndirectObject major _minor _object)                 = major
    getMajor (PDFIndirectObjectWithStream major _minor _dict _stream) = major
    getMajor _                                                        = 0
