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
  , getAdditionalGStates
  , getReference
  , loadFullObject
  , modifyIndirectObjectsP
  , setAdditionalGStates
  )
import Data.PDF.Resource (Resource, createSet, toResource)
import Data.PDF.ResourceDictionary (ResourceDictionary)
import Data.PDF.WorkData (WorkData (wPDF))
import Data.Set (Set)

import PDF.Object.Object (PDFObject (PDFIndirectObjectWithStream), hasKey)
import PDF.Object.Object.Properties (getValueForKey)

import Util.Dictionary (Dictionary)


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
Add generated graphics states to each resource scope without merging unrelated
fonts, XObjects, or other resources from different pages and forms.
-}
updateWithAdditionalResources :: Logging IO => PDFWork IO ()
updateWithAdditionalResources = do
  additional <- getAdditionalGStates

  if Map.null additional
    then return ()
    else do
      modifyIndirectObjectsP (modifyResources additional)
      setAdditionalGStates mempty

 where
  modifyResources
    :: Monad m
    => ResourceDictionary
    -> PDFObject
    -> PDFWork m PDFObject
  modifyResources additional object = case object of
    PDFIndirectObject major minor (PDFDictionary dictionary) ->
      PDFIndirectObject major minor
        . PDFDictionary <$> updateDictionary additional dictionary

    PDFIndirectObjectWithStream major minor dictionary stream -> do
      updated <- updateDictionary additional dictionary
      return (PDFIndirectObjectWithStream major minor updated stream)

    _other -> return object

  updateDictionary
    :: Monad m
    => ResourceDictionary
    -> Dictionary PDFObject
    -> PDFWork m (Dictionary PDFObject)
  updateDictionary additional dictionary =
    case Map.lookup "Resources" dictionary of
      Nothing -> return dictionary
      Just resources -> do
        resolved <- loadObject resources
        case resolved of
          PDFDictionary resourceDictionary -> do
            existing <- maybe (return (PDFDictionary mempty)) loadObject
                          (Map.lookup "ExtGState" resourceDictionary)
            let states = case existing of
                  PDFDictionary values -> values
                  _other               -> mempty

                updated = Map.insert
                            "ExtGState"
                            (PDFDictionary (additional <> states))
                            resourceDictionary

            return (Map.insert "Resources" (PDFDictionary updated) dictionary)

          _other -> return dictionary

  -- Resolve only the dictionary itself, preserving references to its resources.
  loadObject :: Monad m => PDFObject -> PDFWork m PDFObject
  loadObject reference@PDFReference{} = do
    object <- getReference reference

    case object of
      PDFIndirectObject _major _minor value -> return value
      _other                                -> return object

  loadObject object = return object
