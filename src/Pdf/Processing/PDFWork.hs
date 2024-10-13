module Pdf.Processing.PDFWork
  ( importObjects
  , clean
  , pModifyIndirectObjects
  , pMapP
  , deepMapP
  )
where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.State (MonadIO (liftIO), StateT, get, gets, put)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State (evalStateT)

import Data.Fallible (FallibleT)
import Data.Functor ((<&>))
import Data.IntMap qualified as IM
import Data.Kind (Type)
import Data.Logging (Logging)
import Data.Map qualified as Map
import Data.PDF.PDFDocument (PDFDocument)
import Data.PDF.PDFObject
    ( PDFObject (PDFArray, PDFDictionary, PDFIndirectObject, PDFIndirectObjectWithStream, PDFObjectStream)
    )
import Data.PDF.PDFPartition
    ( PDFPartition (ppObjectsWithStream, ppObjectsWithoutStream)
    , ppObjectsWithStream
    , ppObjectsWithoutStream
    )
import Data.PDF.PDFWork (modifyPDF, sayComparisonP, sayErrorP, throwError, tryP)
import Data.PDF.WorkData (WorkData (wPDF))

import Pdf.Document.ObjectStream (explodeDocument)
import Pdf.Document.PDFPartition (partitionDocument, removeUnused)
import Pdf.Object.State (embedObject)

type PDFWork :: (Type -> Type) -> Type -> Type
type PDFWork m a = StateT WorkData (FallibleT m) a

importObjects :: Logging m => PDFDocument -> PDFWork m ()
importObjects objects = do
  workData <- get
  explodedObjects <- explodeDocument objects
  put $ workData { wPDF = partitionDocument explodedObjects}

{- |
Cleans a `PDFPartition` by removing unused objects and logs the number of
removed objects.
-}
clean :: Logging m => PDFWork m ()
clean = do
  pdf <- gets wPDF
  tryP (removeUnused pdf) >>= \case
    Right unusedRemoved -> do
      let allCount = IM.size (ppObjectsWithStream pdf)
                   + IM.size (ppObjectsWithoutStream pdf)
          usedCount = IM.size (ppObjectsWithStream unusedRemoved)
                    + IM.size (ppObjectsWithoutStream unusedRemoved)
      sayComparisonP "Unused objects removal" allCount usedCount
      modifyPDF (const unusedRemoved)
    Left theError -> do
      sayErrorP "Unable to remove unused objects" theError

{- |
Parallel map function that applies a given transformation to each element of a
traversable structure (e.g., list) using concurrency.

Returns the transformed elements wrapped in a `FallibleT`.
-}
pMapP
  :: (Logging IO, Traversable t)
  => (a -> PDFWork IO b)
  -> t a
  -> PDFWork IO (t b)
pMapP transform items = do
  workData <- get
  liftIO (mapConcurrently (\item -> runExceptT (evalStateT (transform item)
                                                           workData
                                               )
                          )
                          items
         )
    >>= either throwError return . sequence

pModifyIndirectObjects
  :: Logging IO
  => (PDFObject -> PDFWork IO PDFObject)
  -> PDFWork IO ()
pModifyIndirectObjects func = do
  pdf <- gets wPDF

  woStream <- pMapP func (ppObjectsWithoutStream pdf)
  wStream  <- pMapP func (ppObjectsWithStream pdf)

  modifyPDF $ const
    pdf { ppObjectsWithStream = wStream, ppObjectsWithoutStream = woStream }

{- |
Apply a function to any object contained by an object at any level.
-}
deepMapP
  :: Logging m
  => (PDFObject -> PDFWork m PDFObject)
  -> PDFObject
  -> PDFWork m PDFObject
deepMapP fn container = case container of
  PDFIndirectObject _ _ object ->
    deepMapP fn object >>= flip embedObject container
  PDFIndirectObjectWithStream _ _ dict _ ->
    deepMapP fn (PDFDictionary dict) >>= flip embedObject container
  PDFObjectStream _ _ dict _ ->
    deepMapP fn (PDFDictionary dict) >>= flip embedObject container
  PDFDictionary dict ->
    sequence (Map.map (deepMapP fn) dict)
      >>= flip embedObject container
      .   PDFDictionary
  PDFArray items -> mapM (deepMapP fn) items <&> PDFArray
  object         -> fn object
