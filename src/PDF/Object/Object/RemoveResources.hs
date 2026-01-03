{-|
Removal of unused resources from PDF objects

This module provides functions to remove unused resources from PDF objects.
Resources are objects referenced in content streams (fonts, images, graphics
states, etc.). This module filters out resource definitions that are not
actually used in the document, reducing file size.
-}
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

{-|
Test whether a resource entry should be retained based on usage.

This function checks if a resource (identified by its type and key) is in the
set of used resources. If it is used, the entry is retained; otherwise it is
filtered out. Unknown resource types are always retained as a safety measure.

Supported resource types: @ColorSpace@, @Font@, @XObject@, @ExtGState@,
@Properties@, @Pattern@, @Shading@, @ProcSet@.

__Parameters:__

- Set of resources known to be used in the document
- The resource type name (e.g., @\"Font\"@)
- The key-value pair to test

__Returns:__

- 'Just' with the item if the resource is used or of unknown type
- 'Nothing' if the resource is not used
-}
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

{-|
Filter a resource type dictionary to remove unused resources.

This function takes a resource type and its associated dictionary (containing
individual resources of that type) and removes entries for resources that are
not in the used set. For non-dictionary resource values, they are passed through
unchanged.

__Parameters:__

- Set of used resources
- A key-value pair where the key is a resource type and the value is a
  dictionary

__Returns:__ The same key-value pair with the dictionary filtered to contain
only used resources.
-}
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

{-|
Filter all resource type dictionaries to remove unused resources.

This function applies resource filtering across all resource types in a
Resources dictionary. It processes each resource type dictionary (Font, XObject,
ColorSpace, etc.) and removes resources that are not in the used set.

__Parameters:__

- Set of resources known to be used
- A complete Resources dictionary

__Returns:__ A new Resources dictionary with unused resources removed from all
resource type categories.
-}
removeResourcesInDictionary
  :: Set Resource
  -> Dictionary PDFObject
  -> Dictionary PDFObject
removeResourcesInDictionary used =
  Map.fromList . fmap (removeByResourceType used) . Map.toList

{-|
Remove unused resources from a PDF object.

This is the main function that removes resource definitions from PDF objects
(pages, streams, etc.) that are not actually used in the document. It handles
various PDF object types and removes unused entries from their Resources
dictionaries.

The function processes:

- Direct dictionary objects with a @Resources@ entry
- Indirect objects containing dictionaries with @Resources@
- Indirect objects with embedded streams that have @Resources@

For page trees and content streams, resources not referenced in the content are
removed, reducing file size without affecting rendering.

__Parameters:__

- Set of resources known to be used in the document
- Set of object numbers that contain resource dictionaries
- The PDF object to process

__Returns:__ A new PDF object with unused resources removed, or the original
object unchanged if it has no Resources dictionary or no unused resources.
-}
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
