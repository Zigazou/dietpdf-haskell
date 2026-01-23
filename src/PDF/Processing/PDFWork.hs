{-|
Helpers for running `PDFWork` transformations over PDF structures.

This module provides utilities to:

* import and partition PDF documents into working state,
* clean unused objects while reporting progress,
* apply transformations concurrently over collections,
* map transformations across nested object structures.

All functions integrate with `PDFWork` and `Logging` to maintain context and
error reporting.
-}
module PDF.Processing.PDFWork
  ( importObjects
  , removeUnusedObjects
  , pModifyIndirectObjects
  , pMapP
  , deepMapP
  , deepMapKeysP
  )
where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.State (MonadIO (liftIO), StateT, get, gets, put)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State (evalStateT)

import Data.ByteString (ByteString)
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

import PDF.Document.ObjectStream (explodeDocument)
import PDF.Document.PDFPartition (partitionDocument, removeUnused)
import PDF.Object.State (embedObject)

{-|
A monad transformer stack for PDF processing operations.

`PDFWork m a` provides a context for applying transformations to PDF documents
while maintaining state and handling errors. It combines `StateT` for managing
`WorkData` (the current PDF state) with `FallibleT` for error handling.

This transformer enables:
  * Stateful manipulation of PDF structures
  * Concurrent processing of PDF objects
  * Error propagation and recovery
  * Logging of operations and comparisons

Type parameters:
  * @m@ - The underlying monad (typically 'IO' for concurrent operations)
  * @a@ - The result type of the computation
-}
type PDFWork :: (Type -> Type) -> Type -> Type
type PDFWork m a = StateT WorkData (FallibleT m) a

{-|
Import a `PDFDocument` into the current `PDFWork` state by exploding object
streams and partitioning into a `PDFPartition`.
-}
importObjects :: Logging m => PDFDocument -> PDFWork m ()
importObjects objects = do
  workData <- get
  explodedObjects <- explodeDocument objects
  put $ workData { wPDF = partitionDocument explodedObjects}

{-|
Cleans a `PDFPartition` by removing unused objects and logs the number of
removed objects.
-}
removeUnusedObjects :: Logging m => PDFWork m ()
removeUnusedObjects = do
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

{-|
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

{-|
Apply a transformation to all indirect objects in the partition, processing both
with-stream and without-stream objects concurrently.
-}
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

{-|
Apply a function to any object contained by an object at any level.
-}
deepMapP
  :: Logging m
  => (PDFObject -> PDFWork m PDFObject)
  -> PDFObject
  -> PDFWork m PDFObject
deepMapP fn container = case container of
  PDFIndirectObject _number _version object ->
    deepMapP fn object >>= flip embedObject container

  PDFIndirectObjectWithStream _number _version dict _stream ->
    deepMapP fn (PDFDictionary dict) >>= flip embedObject container

  PDFObjectStream _number _version dict _stream ->
    deepMapP fn (PDFDictionary dict) >>= flip embedObject container

  PDFDictionary dict ->
    sequence (Map.map (deepMapP fn) dict)
      >>= flip embedObject container
      .   PDFDictionary

  PDFArray items -> mapM (deepMapP fn) items <&> PDFArray

  object -> fn object

{-|
Apply a function to any object contained by an object at any level, while
tracking the path of dictionary keys leading to each object.

This function recursively traverses a PDF object structure and applies a
transformation function to each contained object. Unlike `deepMapP`, it
accumulates the dictionary keys encountered during traversal, allowing the
transformation function to know the "path" through nested dictionaries.

The key path is built in reverse order (most recent key first), with each
dictionary key prepended as the traversal descends into nested structures.

Parameters:
  * @keys@ - The accumulated path of dictionary keys from parent structures
  * @fn@ - Transformation function receiving the key path and object to
    transform
  * @container@ - The PDF object to traverse and transform

Example use case: Finding and modifying objects at specific dictionary paths,
such as updating all image resources regardless of their nesting depth.
-}
deepMapKeysP
  :: Logging m
  => [ByteString]
  -> ([ByteString] -> PDFObject -> PDFWork m PDFObject)
  -> PDFObject
  -> PDFWork m PDFObject
deepMapKeysP keys fn container = case container of
  PDFIndirectObject _number _version object ->
    deepMapKeysP keys fn object >>= flip embedObject container

  PDFIndirectObjectWithStream _number _version dict _stream ->
    deepMapKeysP keys fn (PDFDictionary dict) >>= flip embedObject container

  PDFObjectStream _number _version dict _stream ->
    deepMapKeysP keys fn (PDFDictionary dict) >>= flip embedObject container

  PDFDictionary dict ->
    sequence (Map.mapWithKey (flip deepMapKeysP fn . (: keys)) dict)
      >>= flip embedObject container
      .   PDFDictionary

  PDFArray items -> mapM (deepMapKeysP keys fn) items <&> PDFArray

  object -> fn keys object
