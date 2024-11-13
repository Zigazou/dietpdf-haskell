module Data.PDF.PDFWork
  ( PDFWork
  , modifyPDF
  , evalPDFWork
  , evalPDFWorkT
  , pushContext
  , tryP
  , lastObjectNumber
  , modifyIndirectObjects
  , setTrailer
  , withoutStreamCount
  , getTrailer
  , getObject
  , putObject
  , isEmptyPDF
  , hasNoVersion
  , withStreamCount
  , getReference
  , sayP
  , withContext
  , throwError
  , sayComparisonP
  , sayErrorP
  , fallibleP
  , setTranslationTable
  , getTranslationTable
  , setMasks
  , getMasks
  , putNewObject
  , createNewName
  , getAdditionalGStates
  , addAdditionalGState
  , setAdditionalGStates
  , modifyIndirectObjectsP
  , loadFullObject
  , flattenObject
  )
where

import Control.Monad.State (StateT, get, gets, put)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (runExceptT, throwE)
import Control.Monad.Trans.State (evalStateT)

import Data.ByteString (ByteString)
import Data.Context (Context (NoContext))
import Data.Fallible (Fallible, FallibleT)
import Data.Functor ((<&>))
import Data.IntMap qualified as IM
import Data.Kind (Type)
import Data.Logging (Logging, sayComparisonF, sayErrorF, sayF)
import Data.Map qualified as Map
import Data.PDF.PDFDocument (singleton)
import Data.PDF.PDFObject
  ( PDFObject (PDFArray, PDFDictionary, PDFIndirectObject, PDFIndirectObjectWithGraphics, PDFIndirectObjectWithStream, PDFNull, PDFNumber, PDFReference, PDFTrailer, PDFXRefStream)
  )
import Data.PDF.PDFObjects (findLast)
import Data.PDF.PDFPartition
  ( PDFPartition (ppHeads, ppObjectsWithStream, ppObjectsWithoutStream, ppTrailers)
  , lastTrailer
  )
import Data.PDF.Resource (Resource (ResExtGState), resName, toNameBase)
import Data.PDF.ResourceDictionary (ResourceDictionary)
import Data.PDF.WorkData
  ( WorkData (wAdditionalGStates, wContexts, wMasks, wNameTranslations, wPDF)
  , emptyWorkData
  )
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.TranslationTable (TranslationTable)
import Data.UnifiedError (UnifiedError)

import PDF.Object.Object.Properties (isCatalog, isInfo)

import Util.Dictionary (Dictionary, findFirst, mkDictionary)

type PDFWork :: (Type -> Type) -> Type -> Type
type PDFWork m a = StateT WorkData (FallibleT m) a

evalPDFWork :: Monad m => PDFWork m a -> FallibleT m a
evalPDFWork action = evalStateT action emptyWorkData

evalPDFWorkT :: Monad m => PDFWork m a -> m (Fallible a)
evalPDFWorkT = runExceptT . evalPDFWork

fallibleP :: Monad m => Fallible a -> PDFWork m a
fallibleP fallible = case fallible of
  Right a -> return a
  Left  e -> throwError e

throwError :: Monad m => UnifiedError -> PDFWork m a
throwError err = lift (throwE err)

modifyWorkData :: Monad m => (WorkData -> WorkData) -> PDFWork m ()
modifyWorkData f = get >>= put . f

modifyPDF :: Monad m => (PDFPartition -> PDFPartition) -> PDFWork m ()
modifyPDF f = do
  pdf <- gets wPDF
  modifyWorkData $ \workData -> workData { wPDF = f pdf }

pushContext :: Monad m => Context -> PDFWork m ()
pushContext context = modifyWorkData $ \workData -> workData { wContexts = context : wContexts workData }

popContext :: Monad m => PDFWork m ()
popContext = modifyWorkData $ \workData -> workData
  { wContexts = case wContexts workData of
      []                     -> []
      _currentContext : rest -> rest
  }

currentContext :: Monad m => PDFWork m Context
currentContext = gets wContexts >>= \case
  []            -> return NoContext
  (context : _) -> return context

withContext :: Monad m => Context -> PDFWork m a -> PDFWork m a
withContext context action = do
  pushContext context
  result <- action
  popContext
  return result

sayP :: Logging m => Text -> PDFWork m ()
sayP message = do
  context <- currentContext
  lift (sayF context message)

sayComparisonP :: Logging m => Text -> Int -> Int -> PDFWork m ()
sayComparisonP message before after = do
  context <- currentContext
  lift (sayComparisonF context message before after)

sayErrorP :: Logging m => Text -> UnifiedError -> PDFWork m ()
sayErrorP message theError = do
  context <- currentContext
  lift (sayErrorF context message theError)

getObject :: Monad m => Int -> PDFWork m (Maybe PDFObject)
getObject objectNumber = do
  gets (IM.lookup objectNumber . ppObjectsWithStream . wPDF) >>= \case
    Just object -> return (Just object)
    Nothing ->
      gets (IM.lookup objectNumber . ppObjectsWithoutStream . wPDF) >>= \case
        Just object -> return (Just object)
        Nothing -> return Nothing

{- |
Retrieves the object referenced by a reference object.

If the object is not found, it returns `PDFNull` according to PDF
specifications: "An indirect reference to an undefined object shall not be
considered an error by a PDF processor; it shall be treated as a reference to
the null object."
-}
getReference :: Monad m => PDFObject -> PDFWork m PDFObject
getReference (PDFReference objectNumber _anyVersion) =
  getObject objectNumber >>= \case
    Just object -> return object
    Nothing     -> return PDFNull
getReference (PDFNumber objectNumber) =
  getObject (round objectNumber) >>= \case
    Just object -> return object
    Nothing     -> return PDFNull
getReference _anythingElse = return PDFNull

loadDictionary
  :: Monad m
  => Set Int
  -> Dictionary PDFObject
  -> PDFWork m (Dictionary PDFObject)
loadDictionary alreadySeen dictionary =
  mapM go (Map.toList dictionary) <&> Map.fromList
 where
  go :: Monad m => (ByteString, PDFObject) -> PDFWork m (ByteString, PDFObject)
  go (key, value) = do
    newValue <- loadFullObject' alreadySeen value
    return (key, newValue)

loadFullObject' :: Monad m => Set Int -> PDFObject -> PDFWork m PDFObject
loadFullObject' alreadySeen reference@(PDFReference major _minor) =
  if Set.member major alreadySeen
    then return reference
    else
      getReference reference >>= loadFullObject' alreadySeen

loadFullObject' alreadySeen (PDFArray items) =
  mapM (loadFullObject' alreadySeen) items <&> PDFArray

loadFullObject' alreadySeen (PDFDictionary dictionary) =
  loadDictionary alreadySeen dictionary <&> PDFDictionary

loadFullObject' alreadySeen object@(PDFIndirectObjectWithStream major minor dict stream) =
  if Set.member major alreadySeen
    then return object
    else do
      let alreadySeen' = Set.insert major alreadySeen
      loadedDictionary <- loadDictionary alreadySeen' dict
      return (PDFIndirectObjectWithStream major minor loadedDictionary stream)

loadFullObject' alreadySeen object@(PDFIndirectObject major minor value) =
  if Set.member major alreadySeen
    then return object
    else
      let alreadySeen' = Set.insert major alreadySeen
      in loadFullObject' alreadySeen' value <&> PDFIndirectObject major minor

loadFullObject' _alreadySeen object = return object

loadFullObject :: Monad m => PDFObject -> PDFWork m PDFObject
loadFullObject = loadFullObject' mempty

flattenObject :: PDFObject -> PDFObject
flattenObject (PDFIndirectObject _major _minor value) = flattenObject value
flattenObject (PDFIndirectObjectWithStream major minor _dict _stream) =
  PDFReference major minor
flattenObject (PDFIndirectObjectWithGraphics major minor _dict _gfxObjects) =
  PDFReference major minor
flattenObject (PDFArray items)     = PDFArray (flattenObject <$> items)
flattenObject (PDFDictionary dict) = PDFDictionary (flattenObject <$> dict)
flattenObject object               = object

putObject :: Monad m => PDFObject -> PDFWork m ()
putObject object = modifyPDF $ \pdf ->
  case object of
    PDFIndirectObjectWithStream objectNumber _ _ _ ->
      pdf { ppObjectsWithStream = IM.insert objectNumber
                                            object
                                            (ppObjectsWithStream pdf)
          }
    PDFIndirectObject objectNumber _ _ ->
      pdf { ppObjectsWithoutStream = IM.insert objectNumber
                                               object
                                               (ppObjectsWithoutStream pdf)
          }
    PDFIndirectObjectWithGraphics objectNumber _ _ _ ->
      pdf { ppObjectsWithStream = IM.insert objectNumber
                                            object
                                            (ppObjectsWithStream pdf)
          }
    _anythingElse -> pdf

putNewObject :: Monad m => PDFObject -> PDFWork m Int
putNewObject object = do
  objectNumber <- (+ 1) <$> lastObjectNumber

  case object of
    (PDFIndirectObjectWithStream _major _minor dict stream) -> do
      putObject (PDFIndirectObjectWithStream objectNumber 0 dict stream)
      return objectNumber

    (PDFIndirectObject _major _minor obj) -> do
      putObject (PDFIndirectObject objectNumber 0 obj)
      return objectNumber

    (PDFIndirectObjectWithGraphics _major _minor dict gfxObjects)  -> do
      putObject (PDFIndirectObjectWithGraphics objectNumber 0 dict gfxObjects)
      return objectNumber

    _anythingElse -> return 0

setTranslationTable :: Monad m => TranslationTable Resource -> PDFWork m ()
setTranslationTable translationTable = modifyWorkData $
  \workData -> workData { wNameTranslations = translationTable }

getTranslationTable :: Monad m => PDFWork m (TranslationTable Resource)
getTranslationTable = gets wNameTranslations

createNewName :: (Monad m) => Resource -> PDFWork m Resource
createNewName resourceType = do
  translationTable <- getTranslationTable
  let newName = toNameBase resourceType (Map.size translationTable)
  setTranslationTable (Map.insert newName newName translationTable)
  return newName

setAdditionalGStates :: Monad m => ResourceDictionary -> PDFWork m ()
setAdditionalGStates additionalGStates =
  modifyWorkData $ \workData ->
    workData { wAdditionalGStates = additionalGStates }

getAdditionalGStates :: Monad m => PDFWork m ResourceDictionary
getAdditionalGStates = gets wAdditionalGStates

addAdditionalGState :: Monad m => PDFObject -> PDFWork m Resource
addAdditionalGState additionalGState = do
  currentGStates <- getAdditionalGStates

  key <- case findFirst (== additionalGState) currentGStates of
          Just key -> return (ResExtGState key)
          Nothing  -> createNewName (ResExtGState "")

  let newGStates = Map.insert (resName key) additionalGState currentGStates
  setAdditionalGStates newGStates

  return key

setMasks :: Monad m => Set Int -> PDFWork m ()
setMasks masks = modifyWorkData $ \workData -> workData { wMasks = masks }

getMasks :: Monad m => PDFWork m (Set Int)
getMasks = gets wMasks

isEmptyPDF :: Monad m => PDFWork m Bool
isEmptyPDF = do
  noObjectWithStream    <- gets (null . ppObjectsWithStream . wPDF)
  noObjectWithoutStream <- gets (null . ppObjectsWithoutStream . wPDF)
  return (noObjectWithStream && noObjectWithoutStream)

hasNoVersion :: Monad m => PDFWork m Bool
hasNoVersion = gets (null . ppHeads . wPDF)

withStreamCount :: Monad m => PDFWork m Int
withStreamCount = gets (IM.size . ppObjectsWithStream . wPDF)

withoutStreamCount :: Monad m => PDFWork m Int
withoutStreamCount = gets (IM.size . ppObjectsWithoutStream . wPDF)

{- |
Retrieves the trailer object. If a valid trailer object is not present, it
attempts to create one using the "Root" and "Info" references from the
partition.

Returns the final trailer object.
-}
getTrailer :: Monad m => PDFWork m PDFObject
getTrailer = do
  partition <- gets wPDF

  case lastTrailer partition of
    (PDFTrailer PDFNull) ->
      let
        catalog = findLast isCatalog (ppObjectsWithoutStream partition)
        info    = findLast isInfo    (ppObjectsWithoutStream partition)
      in
        case (catalog, info) of
          ( Just (PDFIndirectObject cNumber cRevision _),
            Just (PDFIndirectObject iNumber iRevision _)) -> return
              $ PDFTrailer (PDFDictionary $ mkDictionary
                             [ ("Root", PDFReference cNumber cRevision)
                             , ("Info", PDFReference iNumber iRevision)
                             ]
                           )
          (Just (PDFIndirectObject cNumber cRevision _), Nothing) -> return
            $ PDFTrailer ( PDFDictionary
                         $ mkDictionary [("Root", PDFReference cNumber cRevision)]
                         )
          _anyOtherCase -> return $ PDFTrailer PDFNull
    (PDFXRefStream _ _ dict _) ->
      let catalog = Map.lookup "Root" dict
          info    = Map.lookup "Info" dict
      in  case (catalog, info) of
            (Just rCatalog, Just rInfo) -> return
              $ PDFTrailer ( PDFDictionary
                           $ mkDictionary [("Root", rCatalog), ("Info", rInfo)]
                           )
            _anyOtherCase -> return $ PDFTrailer PDFNull
    validTrailer -> return validTrailer

setTrailer :: Monad m => PDFObject -> PDFWork m ()
setTrailer trailer = modifyPDF $ \pdf -> pdf { ppTrailers = singleton trailer }

modifyIndirectObjects :: Monad m => (PDFObject -> PDFObject) -> PDFWork m ()
modifyIndirectObjects func = modifyPDF $ \pdf ->
  pdf { ppObjectsWithStream    = IM.map func (ppObjectsWithStream pdf)
      , ppObjectsWithoutStream = IM.map func (ppObjectsWithoutStream pdf)
      }

modifyIndirectObjectsP
  :: Monad m
  => (PDFObject -> PDFWork m PDFObject)
  -> PDFWork m ()
modifyIndirectObjectsP func = do
  origin <- gets wPDF
  os <- mapM func (ppObjectsWithStream origin)
  ows <- mapM func (ppObjectsWithoutStream origin)
  modifyPDF $ \pdf -> pdf { ppObjectsWithStream = os
                          , ppObjectsWithoutStream = ows
                          }

lastObjectNumber :: Monad m => PDFWork m Int
lastObjectNumber = do
  pdf <- gets wPDF
  let lastWithStream    = fst . IM.findMax $ ppObjectsWithStream pdf
      lastWithoutStream = fst . IM.findMax $ ppObjectsWithoutStream pdf

  return (max lastWithStream lastWithoutStream)

tryP :: Logging m => PDFWork m a -> PDFWork m (Fallible a)
tryP = (get >>=) . ((lift . lift . runExceptT) .) . evalStateT
