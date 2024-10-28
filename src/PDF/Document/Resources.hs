module PDF.Document.Resources
  ( getAllResourceNames
  ) where

import Control.Monad (foldM)
import Control.Monad.State (gets)

import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.PDF.PDFDocument (deepFind)
import Data.PDF.PDFObject
  ( PDFObject (PDFDictionary, PDFIndirectObject, PDFIndirectObjectWithStream, PDFReference)
  )
import Data.PDF.PDFObjects (toPDFDocument)
import Data.PDF.PDFPartition (PDFPartition (ppObjectsWithoutStream))
import Data.PDF.PDFWork (PDFWork, getReference)
import Data.PDF.Resource (Resource, createSet, toResource)
import Data.PDF.WorkData (WorkData (wPDF))
import Data.Set (Set)

import PDF.Object.Object (hasKey)
import PDF.Object.Object.Properties (getValueForKey)

getResourceKeys
  :: Monad m
  => ByteString
  -> PDFObject
  -> PDFWork m (Set Resource)
getResourceKeys key object =
  case getValueForKey key object of
    Just (PDFDictionary dict) ->
      return $ createSet (toResource key <$> Map.keys dict)
    _notFound -> return mempty

getResourceKeysFromDictionary
  :: Monad m
  => PDFObject
  -> PDFWork m (Set Resource)
getResourceKeysFromDictionary dictionary = do
  sColorSpace <- getResourceKeys "ColorSpace" dictionary
  sFont       <- getResourceKeys "Font" dictionary
  sXObject    <- getResourceKeys "XObject" dictionary
  sExtGState  <- getResourceKeys "ExtGState" dictionary
  sProperties <- getResourceKeys "Properties" dictionary
  sPattern    <- getResourceKeys "Pattern" dictionary
  sShading    <- getResourceKeys "Shading" dictionary
  sProcSet    <- getResourceKeys "ProcSet" dictionary

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
  :: Monad m
  => Set Resource
  -> PDFObject
  -> PDFWork m (Set Resource)
getResourceNames resources reference@PDFReference{} = do
  subObject <- getReference reference
  newResources <- getResourceNames resources subObject
  return (resources <> newResources)

getResourceNames resources (PDFIndirectObject _ _ eObject) = do
  newResources <- getResourceNames resources eObject
  return (resources <> newResources)

getResourceNames resources (PDFIndirectObjectWithStream _ _ eObject _) = do
  newResources <- getResourceNames resources (PDFDictionary eObject)
  return (resources <> newResources)

getResourceNames resources object = case getValueForKey "Resources" object of
  Just reference@PDFReference{} -> do
    subObject <- getReference reference
    newResources <- getResourceNames resources subObject
    return (resources <> newResources)

  Just dictionary@PDFDictionary{} -> do
    newResources <- getResourceKeysFromDictionary dictionary
    return (resources <> newResources)

  _notFound -> do
    newResources <- getResourceKeysFromDictionary object
    return (resources <> newResources)

{- |
Finds all resource names in a collection of PDF objects.

Resources are typically stored in a dictionary object with a "Resources" key.
-}
getAllResourceNames :: Monad m => PDFWork m (Set Resource)
getAllResourceNames = do
  gets ( deepFind (hasKey "Resources")
       . toPDFDocument
       . ppObjectsWithoutStream
       . wPDF
       ) >>= foldM getResourceNames mempty
