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

{- |
Finds all resource names in a collection of PDF objects.

Resources are typically stored in a dictionary object with a "Resources" key.
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

createResources :: Logging m => PDFWork m PDFObject
createResources = do
  currentResources <- getAllResources
  newExtGStates <- getAdditionalGStates

  let newResources = mergeResourceDictionaries
        currentResources
        (Map.singleton "ExtGState" newExtGStates)

  major <- putNewObject (PDFIndirectObject 0 0 (resourceDictionariesToPDFObject newResources))
  return (PDFReference major 0)

modifyResources :: Monad m => PDFObject -> PDFObject -> PDFWork m PDFObject
modifyResources newResources object@(PDFIndirectObject major minor (PDFDictionary dictionary)) =
  if dictHasKey "Resources" dictionary
    then do
      let newDictionary = Map.insert "Resources" newResources dictionary
      return $ PDFIndirectObject major minor (PDFDictionary newDictionary)
    else
      return object
modifyResources _newResources object = return object

updateWithAdditionalResources :: Logging IO => PDFWork IO ()
updateWithAdditionalResources = do
  newResources <- createResources
  setAdditionalGStates mempty
  modifyIndirectObjectsP (modifyResources newResources)
