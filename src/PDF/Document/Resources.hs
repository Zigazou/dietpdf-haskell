{-|
Manage PDF resource dictionaries and names.

Provides utilities for extracting resource names and dictionaries from PDF
objects, merging resources, and updating documents with additional resources
such as graphics states created during optimization.
-}
module PDF.Document.Resources
  ( getAllResourceNames
  , removeUnusedResources
  , updateWithAdditionalResources
  ) where

import Control.Monad (foldM)
import Control.Monad.State (gets)

import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Data.IntMap qualified as IM
import Data.Logging (Logging)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, isJust)
import Data.PDF.GFXObject
  (GFXObject (GFXArray, GFXDictionary, GFXInlineImage, GFXName))
import Data.PDF.PDFDocument (cFilter)
import Data.PDF.PDFObject
  ( PDFObject (PDFArray, PDFDictionary, PDFIndirectObject, PDFIndirectObjectWithStream, PDFName, PDFNull, PDFReference)
  )
import Data.PDF.PDFObjects (toPDFDocument)
import Data.PDF.PDFPartition
  (PDFPartition (ppObjectsWithStream, ppObjectsWithoutStream))
import Data.PDF.PDFWork
  ( PDFWork
  , getAdditionalGStates
  , getReference
  , modifyIndirectObjectsP
  , setAdditionalGStates
  , tryP
  )
import Data.PDF.Resource (Resource, createSet, toResource)
import Data.PDF.ResourceDictionary (ResourceDictionary)
import Data.PDF.WorkData (WorkData (wPDF))
import Data.Set (Set)
import Data.Set qualified as Set

import PDF.Graphics.Parser.Stream (gfxParse)
import PDF.Object.Container (getFilters)
import PDF.Object.Object.Properties (getValueForKey, hasKey)
import PDF.Object.Object.RemoveResources (removeResources)
import PDF.Object.State (getStream)
import PDF.Processing.Unfilter (unfilter)

import Util.Dictionary (Dictionary)


{-|
Resolve a single level of indirection, without descending into the resolved
object's own entries. Only the immediate reference is followed, so shared
sub-objects (fonts, XObjects...) are never expanded.
-}
loadShallow :: Monad m => PDFObject -> PDFWork m PDFObject
loadShallow reference@PDFReference{} = do
  object <- getReference reference

  case object of
    PDFIndirectObject _major _minor value -> return value
    _other                                -> return object

loadShallow object = return object

{-|
Extract resource names of a specific type from a PDF object.

Looks for a dictionary stored under the given key (e.g., "Font", "XObject",
"ColorSpace") and returns all dictionary keys wrapped as resources. Only the
dictionary's keys are needed, so its entries (fonts, XObjects...) are resolved
one level at most, never expanded further. Returns an empty set if the key is
absent or the value is not a dictionary.
-}
getResourceKeys
  :: Logging m
  => ByteString
  -> PDFObject
  -> PDFWork m (Set Resource)
getResourceKeys key object = do
  resolved <- maybe (return PDFNull) loadShallow (getValueForKey key object)
  case resolved of
    PDFDictionary dict ->
      return $ createSet (toResource key <$> Map.keys dict)

    PDFIndirectObjectWithStream _major _minor dict _stream ->
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
getResourceNames resources object =
  case getValueForKey "Resources" object of
    Just value -> do
      loadedValue <- loadShallow value
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
            let
              states :: Dictionary PDFObject
              states = case existing of
                PDFDictionary values -> values
                _other               -> mempty

              updated :: Dictionary PDFObject
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

{-|
Remove resource entries whose names never occur in graphics or object values.
The name set is deliberately shared across scopes: collisions retain extra
resources, rather than risking deletion from inherited or shared dictionaries.
Indirect resource and category dictionaries are resolved without expanding the
resource objects themselves. Unknown content leaves resources untouched.
-}
removeUnusedResources :: Logging m => PDFWork m ()
removeUnusedResources = do
  pdf <- gets wPDF
  let objects = toList (toPDFDocument (ppObjectsWithoutStream pdf)
                     <> toPDFDocument (ppObjectsWithStream pdf))
      -- Content streams and Type 3 glyphs need successful parsing even when
      -- their stream dictionaries do not identify them as graphics.
      required = expandReferences
                   (ppObjectsWithoutStream pdf <> ppObjectsWithStream pdf)
                   (Set.unions (map contentReferences objects))
      objectNames = Set.unions (map names objects)
  parsed <- mapM (streamNames required) (toList (ppObjectsWithStream pdf))
  -- Default appearances and Type 3 fonts need additional analysis; retain
  -- their resources conservatively.
  if any needsAppearanceResources objects || elem Nothing parsed
    then return ()
    else do
      let
        usedNames :: Set ByteString
        usedNames = objectNames <> Set.unions (catMaybes parsed)

        used :: Set Resource
        used = createSet
          [toResource category name
          | category <- [ "Font"
                        , "XObject"
                        , "ExtGState"
                        , "ColorSpace"
                        , "Pattern"
                        , "Shading"
                        , "Properties"
                        ]
          , name <- Set.toList usedNames
          ]

      modifyIndirectObjectsP (prune used)
 where
  needsAppearanceResources :: PDFObject -> Bool
  needsAppearanceResources (PDFIndirectObject _ _ value) =
    needsAppearanceResources value

  needsAppearanceResources (PDFIndirectObjectWithStream _ _ dictionary _) =
    needsAppearanceResources (PDFDictionary dictionary)

  needsAppearanceResources object@(PDFDictionary dictionary) =
    hasKey "AcroForm" object
    || hasKey "DA" object
    || getValueForKey "Subtype" object == Just (PDFName "Type3")
    || any needsAppearanceResources dictionary

  needsAppearanceResources (PDFArray values) =
    any needsAppearanceResources values

  needsAppearanceResources _ = False

  names :: PDFObject -> Set ByteString
  names (PDFName name) = Set.singleton name
  names (PDFArray values) = foldMap names values
  names (PDFDictionary dictionary) = foldMap names dictionary
  names (PDFIndirectObject _ _ value) = names value
  names (PDFIndirectObjectWithStream _ _ dictionary _) =
    foldMap names dictionary
  names _ = mempty

  expandReferences :: IM.IntMap PDFObject -> Set Int -> Set Int
  expandReferences objects references =
    let
      children :: Int -> Set Int
      children number = case IM.lookup number objects of
          Just (PDFIndirectObject _ _ value) -> referencesIn value
          _                                  -> mempty

      expanded :: Set Int
      expanded = references <> foldMap children references
    in
      if expanded == references
        then references
        else expandReferences objects expanded

  referencesIn :: PDFObject -> Set Int
  referencesIn (PDFReference number _)    = Set.singleton number
  referencesIn (PDFArray values)          = foldMap referencesIn values
  referencesIn (PDFDictionary dictionary) = foldMap referencesIn dictionary
  referencesIn _                          = mempty

  contentReferences :: PDFObject -> Set Int
  contentReferences object =
    foldMap referencesIn (getValueForKey "Contents" object)
    <> foldMap referencesIn (getValueForKey "CharProcs" object)

  gfxNames :: GFXObject -> Set ByteString
  gfxNames (GFXName name)                = Set.singleton name
  gfxNames (GFXArray values)             = foldMap gfxNames values
  gfxNames (GFXDictionary dictionary)    = foldMap gfxNames dictionary
  gfxNames (GFXInlineImage dictionary _) = foldMap gfxNames dictionary
  gfxNames _                             = mempty

  streamNames
    :: Logging m
    => Set Int
    -> PDFObject
    -> PDFWork m (Maybe (Set ByteString))
  streamNames required object@(PDFIndirectObjectWithStream number _ _ _) = do
    result <- tryP $ do
      decoded <- unfilter object
      (,) <$> getFilters decoded <*> getStream decoded

    let
      mandatory :: Bool
      mandatory =
        Set.member number required
        || getValueForKey "Subtype" object == Just (PDFName "Form")
        || isJust (getValueForKey "PatternType" object)

      failed :: Maybe (Set ByteString)
      failed = if mandatory
                then Nothing
                else Just mempty

    return $ case result of
      Right (filters, stream) | null filters -> case gfxParse stream of
        Right graphics -> Just (foldMap gfxNames graphics)
        Left _         -> failed
      _ -> failed

  streamNames _ _ = return (Just mempty)

  resolve :: Monad m => PDFObject -> PDFWork m PDFObject
  resolve reference@PDFReference{} = do
    object <- getReference reference
    case object of
      PDFIndirectObject _ _ value -> return value
      _                           -> return object
  resolve object = return object

  prune :: Monad m => Set Resource -> PDFObject -> PDFWork m PDFObject
  prune used (PDFIndirectObject number generation value) =
    PDFIndirectObject number generation <$> prune used value

  prune used
        (PDFIndirectObjectWithStream number generation dictionary stream) = do
    updated <- prune used (PDFDictionary dictionary)
    case updated of
      PDFDictionary values ->
        return (PDFIndirectObjectWithStream number generation values stream)
      _anyOtherValue ->
        return (PDFIndirectObjectWithStream number generation dictionary stream)

  prune used (PDFDictionary dictionary) = do
    nested <- traverse (prune used) dictionary
    case Map.lookup "Resources" nested of
      Nothing -> return (PDFDictionary nested)

      Just resources -> do
        resolved <- resolve resources

        case resolved of
          PDFDictionary categories -> do
            direct <- traverse resolve categories
            -- Device color-space substitutions have implicit uses.
            let
              defaults :: Set Resource
              defaults =
                createSet (map (toResource "ColorSpace")
                               ["DefaultGray", "DefaultRGB", "DefaultCMYK"]
                          )

              owner :: PDFObject
              owner = PDFDictionary (Map.insert "Resources"
                          (PDFDictionary direct) nested)

            return (removeResources (used <> defaults) mempty owner)

          _ -> return (PDFDictionary nested)

  prune used (PDFArray values) = PDFArray <$> traverse (prune used) values

  prune _ object               = return object
