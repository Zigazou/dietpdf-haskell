module PDF.Object.Object.RemoveResources
  ( removeResources
  ) where

import Data.ByteString (ByteString)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.PDF.PDFObject
  ( PDFObject (PDFDictionary, PDFIndirectObject, PDFIndirectObjectWithStream)
  )
import Data.PDF.Resource
  ( Resource (ResColorSpace, ResExtGState, ResFont, ResPattern, ResProcSet, ResProperties, ResShading, ResXObject)
  )
import Data.Set (Set)
import Data.Set qualified as Set

import PDF.Object.Object.Properties (getValueForKey, hasKey, setValueForKey)

import Util.Dictionary (Dictionary)

removeIfNotUsed
  :: Set Resource
  -> ByteString
  -> (ByteString, PDFObject)
  -> Maybe (ByteString, PDFObject)
removeIfNotUsed used "ColorSpace" item@(key, _value) =
  if Set.member (ResColorSpace key) used then Just item else Nothing
removeIfNotUsed used "Font" item@(key, _value) =
  if Set.member (ResFont key) used then Just item else Nothing
removeIfNotUsed used "XObject" item@(key, _value) =
  if Set.member (ResXObject key) used then Just item else Nothing
removeIfNotUsed used "ExtGState" item@(key, _value) =
  if Set.member (ResExtGState key) used then Just item else Nothing
removeIfNotUsed used "Properties" item@(key, _value) =
  if Set.member (ResProperties key) used then Just item else Nothing
removeIfNotUsed used "Pattern" item@(key, _value) =
  if Set.member (ResPattern key) used then Just item else Nothing
removeIfNotUsed used "Shading" item@(key, _value) =
  if Set.member (ResShading key) used then Just item else Nothing
removeIfNotUsed used "ProcSet" item@(key, _value) =
  if Set.member (ResProcSet key) used then Just item else Nothing
removeIfNotUsed _used _anyOtherResourceType item = Just item

removeByResourceType
  :: Set Resource
  -> (ByteString, PDFObject)
  -> (ByteString, PDFObject)
removeByResourceType used (resourceType, PDFDictionary dict) =
  ( resourceType
  , PDFDictionary ( Map.fromList
                  . mapMaybe (removeIfNotUsed used resourceType)
                  . Map.toList
                  $ dict
                  )
  )
removeByResourceType _used (resourceType, object) = (resourceType, object)

removeResourcesInDictionary
  :: Set Resource
  -> Dictionary PDFObject
  -> Dictionary PDFObject
removeResourcesInDictionary used =
  Map.fromList . fmap (removeByResourceType used) . Map.toList

removeResources :: Set Resource -> Set Int -> PDFObject -> PDFObject
removeResources used _containingResources dictionary@PDFDictionary{} =
  case getValueForKey "Resources" dictionary of
    Just (PDFDictionary resources) -> setValueForKey
        "Resources"
        (Just . PDFDictionary $ removeResourcesInDictionary used resources)
        dictionary
    _anyOtherCase -> dictionary
removeResources used containingResources object@(PDFIndirectObject number revision (PDFDictionary dict))
  | Set.member number containingResources =
      PDFIndirectObject
        number
        revision
        (PDFDictionary (removeResourcesInDictionary used dict))
  | hasKey "Resources" object =
      PDFIndirectObject
        number
        revision
        (removeResources used containingResources (PDFDictionary dict))
  | otherwise = object
removeResources used containingResources object@(PDFIndirectObjectWithStream number revision dict stream)
  | Set.member number containingResources =
      PDFIndirectObjectWithStream
        number
        revision
        (removeResourcesInDictionary used dict)
        stream
  | hasKey "Resources" object =
      PDFIndirectObjectWithStream
        number
        revision
        (removeResourcesInDictionary used dict)
        stream
  | otherwise = object
removeResources _used _containingResources object = object
