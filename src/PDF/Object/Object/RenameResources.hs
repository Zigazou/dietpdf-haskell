{-|
Renaming and tracking of PDF resources

This module provides functions to rename PDF resources to shorter identifiers
for file size reduction, and to identify which objects contain resource
definitions. Resource renaming is part of PDF optimization that maintains visual
correctness while reducing file size.
-}
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

{-|
Extract the major object number from a PDF object.

For references and indirect objects, returns the object number (major version).
For all other object types, returns an empty set.

__Parameters:__

- The PDF object to examine

__Returns:__ A set containing the major object number, or an empty set if the
object is not a reference or indirect object.
-}
getMajor :: PDFObject -> Set Int
getMajor (PDFReference major _) = Set.singleton major
getMajor (PDFIndirectObject major _minor _object) = Set.singleton major
getMajor (PDFIndirectObjectWithStream major _minor _dict _stream) =
  Set.singleton major
getMajor _anyOtherObject = mempty

{-|
Convert a resource name to its renamed form using a translation table.

This function takes a resource identifier (like a font name) and translates it
to a shorter, optimized identifier using the provided translation table. The
translation is performed by constructing a Resource object, looking it up in the
table, and extracting the new name.

__Parameters:__

- Translation table mapping resources to their short names
- Constructor function to create a Resource from a name
- The original resource name

__Returns:__ The shortened resource name.
-}
convertResource
  :: TranslationTable Resource
  -> (ByteString -> Resource)
  -> ByteString
  -> ByteString
convertResource table constructor = resName . convert table . constructor

{-|
Rename a single resource entry based on its type.

This function takes a resource type (like @\"Font\"@ or @\"XObject\"@) and a
key-value pair, and renames the key to its optimized form if the resource type
is recognized. Unknown resource types are passed through unchanged.

__Parameters:__

- Translation table for resource name mapping
- The resource type name
- The key-value pair to rename

__Returns:__ The key-value pair with the key renamed to its optimized form.
-}
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

{-|
Test whether a bytestring is a recognized PDF resource type name.

Recognized resource types include: @ColorSpace@, @Font@, @XObject@, @ExtGState@,
@Properties@, @Pattern@, @Shading@, @ProcSet@.

__Parameters:__

- The bytestring to test

__Returns:__ 'True' if the string is a known resource type, 'False' otherwise.
-}
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

{-|
Rename all resources within a single resource type dictionary.

This function processes a resource type and its associated dictionary, renaming
all resource keys within that dictionary to their optimized forms. For
recognized resource types, entries are renamed according to the translation
table. For unknown types, the dictionary is passed through unchanged.

__Parameters:__

- Translation table for resource name mapping
- A key-value pair where the key is a resource type and the value is a
  dictionary

__Returns:__ The same key-value pair with all resource names in the dictionary
renamed to optimized forms.
-}
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

{-|
Rename all resources within a complete Resources dictionary.

This function recursively processes a Resources dictionary, renaming entries in
all resource type subdictionaries (Font, XObject, ColorSpace, etc.) to their
optimized forms. If the dictionary contains a nested Resources entry, it is
processed recursively.

__Parameters:__

- Translation table for resource name mapping
- A complete Resources dictionary

__Returns:__ A new Resources dictionary with all resource names optimized.
-}
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

{-|
Rename resources in a PDF object to use optimized identifiers.

This is the main function that applies resource renaming to PDF objects. It
handles dictionary objects and indirect objects containing resource
dictionaries. For page content streams and other objects that reference
resources, this function ensures all resource identifiers are shortened
according to the translation table.

__Parameters:__

- Translation table mapping original resource names to optimized names
- Set of object numbers that contain resource definitions
- The PDF object to process

__Returns:__ A new PDF object with all resource names renamed to optimized
forms, or the original object if it contains no resources.
-}
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

{-|
Identify all objects that contain or reference resources in a PDF document.

This function scans a PDF document to find all objects that define or reference
resources (fonts, images, color spaces, graphics states, etc.). It performs a
deep search for objects with Resource dictionaries and collects all object
numbers involved in the resource hierarchy.

__Parameters:__

- The PDF document to scan

__Returns:__ A set of object numbers that contain or reference resources. This
set can be used by resource optimization functions to identify which objects
need resource processing.
-}
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
