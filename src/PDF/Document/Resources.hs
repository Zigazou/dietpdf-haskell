{-|
Manage PDF resource dictionaries and names.

Provides utilities for extracting resource names and dictionaries from PDF
objects, merging resources, and updating documents with additional resources
such as graphics states created during optimization.
-}
module PDF.Document.Resources
  ( getAllResourceNames
  , updateWithAdditionalResources
  ) where

import Control.Monad (foldM)
import Control.Monad.State (gets)

import Data.ByteString (ByteString)
import Data.Functor ((<&>))
import Data.Logging (Logging)
import Data.Map.Strict qualified as Map
import Data.PDF.PDFDocument (cFilter)
import Data.PDF.PDFObject
  (PDFObject (PDFDictionary, PDFIndirectObject, PDFReference))
import Data.PDF.PDFObjects (toPDFDocument)
import Data.PDF.PDFPartition
  (PDFPartition (ppObjectsWithStream, ppObjectsWithoutStream))
import Data.PDF.PDFWork
  ( PDFWork
  , flattenObject
  , getAdditionalGStates
  , loadFullObject
  , modifyIndirectObjectsP
  , putNewObject
  , setAdditionalGStates
  )
import Data.PDF.Resource (Resource, createSet, toResource)
import Data.PDF.ResourceDictionary
  ( ResourceDictionary
  , mergeResourceDictionaries
  , objectToResourceDictionaries
  , resourceDictionariesToPDFObject
  )
import Data.PDF.WorkData (WorkData (wPDF))
import Data.Set (Set)

import PDF.Object.Object (PDFObject (PDFIndirectObjectWithStream), hasKey)
import PDF.Object.Object.Properties (getValueForKey)

import Util.Dictionary (Dictionary, dictHasKey)


{-|
Extract resource names of a specific type from a PDF object.

Looks for a dictionary stored under the given key (e.g., "Font", "XObject",
"ColorSpace") and returns all dictionary keys wrapped as resources. Returns an
empty set if the key is absent or the value is not a dictionary.
-}
getResourceKeys
  :: Logging m
  => ByteString
  -> PDFObject
  -> PDFWork m (Set Resource)
getResourceKeys key object =
  case getValueForKey key object of
    Just (PDFDictionary dict) ->
      return $ createSet (toResource key <$> Map.keys dict)
    Just (PDFIndirectObject _major _minor (PDFDictionary dict)) ->
      return $ createSet (toResource key <$> Map.keys dict)
    Just (PDFIndirectObjectWithStream _major _minor dict _stream) ->
      return $ createSet (toResource key <$> Map.keys dict)
    _notFound -> return mempty

{-|
Extract all resource names from a resource dictionary.

Searches for standard resource types (ColorSpace, Font, XObject, ExtGState,
Properties, Pattern, Shading, ProcSet) and returns the union of all resource
names found in each section.
-}
getResourceKeysFromDictionary
  :: Logging m
  => PDFObject
  -> PDFWork m (Set Resource)
getResourceKeysFromDictionary dictionary = do
  sColorSpace <- getResourceKeys "ColorSpace" dictionary
  sFont       <- getResourceKeys "Font"       dictionary
  sXObject    <- getResourceKeys "XObject"    dictionary
  sExtGState  <- getResourceKeys "ExtGState"  dictionary
  sProperties <- getResourceKeys "Properties" dictionary
  sPattern    <- getResourceKeys "Pattern"    dictionary
  sShading    <- getResourceKeys "Shading"    dictionary
  sProcSet    <- getResourceKeys "ProcSet"    dictionary

  return (   sColorSpace
          <> sFont
          <> sXObject
          <> sExtGState
          <> sProperties
          <> sPattern
          <> sShading
          <> sProcSet
          )

{-|
Accumulate resource names from a PDF object.

Looks for a "Resources" entry in the object, loads the resource dictionary, and
extracts all resource names. Merges them with the provided resource set. Returns
the input set unchanged if no "Resources" entry is found.
-}
getResourceNames
  :: Logging m
  => Set Resource
  -> PDFObject
  -> PDFWork m (Set Resource)
getResourceNames resources object = case getValueForKey "Resources" object of
  Just value -> do
    loadedValue <- loadFullObject value
    names <- getResourceKeysFromDictionary loadedValue
    return (resources <> names)
  _notFound -> return resources

{-|
Find all resource names in the entire PDF document.

Searches both objects with and without streams, collecting all resource names
from entries with a "Resources" key. Returns the union of all resource names
across the document.
-}
getAllResourceNames :: Logging m => PDFWork m (Set Resource)
getAllResourceNames = do
  wosObjects <- gets (toPDFDocument . ppObjectsWithoutStream . wPDF)
  wsObjects <- gets (toPDFDocument . ppObjectsWithStream . wPDF)

  wosResources <- foldM getResourceNames
                        mempty
                        (cFilter (hasKey "Resources") wosObjects)

  wsResources <-  foldM getResourceNames
                        mempty
                        (cFilter (hasKey "Resources") wsObjects)

  return (wosResources <> wsResources)

{-|
Extract all resource dictionaries from the PDF document.

Collects and merges all resource dictionaries from objects in both streams and
non-stream sections. Returns a single unified resource dictionary mapping
resource types to their component dictionaries.
-}
getAllResources :: Logging m => PDFWork m (Dictionary ResourceDictionary)
getAllResources = do
  wosObjects <- gets (toPDFDocument . ppObjectsWithoutStream . wPDF)
  wsObjects <- gets (toPDFDocument . ppObjectsWithStream . wPDF)

  wosResources <- foldM accResources
                        mempty
                        (cFilter (hasKey "Resources") wosObjects)

  wsResources <-  foldM accResources
                        mempty
                        (cFilter (hasKey "Resources") wsObjects)

  return (mergeResourceDictionaries wosResources wsResources)
 where
  accResources
    :: Logging m
    => Dictionary ResourceDictionary
    -> PDFObject
    -> PDFWork m (Dictionary ResourceDictionary)
  accResources resources object = case getValueForKey "Resources" object of
    Just value -> do
      newResources <- loadFullObject value
                  <&> objectToResourceDictionaries . flattenObject

      return (mergeResourceDictionaries resources newResources)
    _notFound  -> return resources

{-|
Create a new resource object with additional graphics states.

Merges the current document resources with any new graphics states that were
generated during optimization. Returns a reference to the new resource object.
-}
createResources :: Logging m => PDFWork m PDFObject
createResources = do
  currentResources <- getAllResources
  newExtGStates <- getAdditionalGStates

  let newResources = mergeResourceDictionaries
        currentResources
        (Map.singleton "ExtGState" newExtGStates)

  major <- putNewObject (PDFIndirectObject 0 0 (resourceDictionariesToPDFObject newResources))
  return (PDFReference major 0)

{-|
Update the Resources entry in an indirect object's dictionary.

If the object is an indirect object with a dictionary containing a "Resources"
key, replaces the resources with the new resource object. Otherwise, returns the
object unchanged.
-}
modifyResources :: Monad m => PDFObject -> PDFObject -> PDFWork m PDFObject
modifyResources newResources object@(PDFIndirectObject major minor (PDFDictionary dictionary)) =
  if dictHasKey "Resources" dictionary
    then do
      let newDictionary = Map.insert "Resources" newResources dictionary
      return $ PDFIndirectObject major minor (PDFDictionary newDictionary)
    else
      return object
modifyResources _newResources object = return object

{-|
Update all objects in the document with additional resources.

Creates a merged resource dictionary that combines existing resources with any
new graphics states or other resources generated during optimization. Updates
all indirect objects with the new resources and clears the temporary additional
graphics states store.
-}
updateWithAdditionalResources :: Logging IO => PDFWork IO ()
updateWithAdditionalResources = do
  newResources <- createResources
  setAdditionalGStates mempty
  modifyIndirectObjectsP (modifyResources newResources)
