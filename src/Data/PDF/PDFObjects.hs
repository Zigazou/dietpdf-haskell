-- |
-- This module manipulates collections of `PDFObject`.
--
-- These functions are meant to be used when optimizing/encoding a PDF.
module Data.PDF.PDFObjects
  ( -- * Encoding of object collections
    PDFObjects,
    toPDFDocument,
    fromPDFDocument,
    findLast,
    insertObject,
  )
where

import Data.IntMap.Strict qualified as IM
import Data.Kind (Type)
import Data.List (find)
import Data.PDF.PDFDocument (PDFDocument, fromList, toList)
import Data.PDF.PDFObject
    ( PDFObject (PDFIndirectObject, PDFIndirectObjectWithGraphics, PDFIndirectObjectWithStream, PDFXRefStream)
    , getObjectNumber
    )

-- | A collection of objects indexed by the object number
type PDFObjects :: Type
type PDFObjects = IM.IntMap PDFObject

toPDFDocument :: PDFObjects -> PDFDocument
toPDFDocument = fromList . fmap snd . IM.toAscList

fromPDFDocument :: PDFDocument -> PDFObjects
fromPDFDocument = IM.fromList . fmap createCouple . toList
  where
    createCouple :: PDFObject -> (Int, PDFObject)
    createCouple object@(PDFIndirectObject number _ _) = (number, object)
    createCouple object@(PDFIndirectObjectWithGraphics number _ _ _) =
      (number, object)
    createCouple object@(PDFIndirectObjectWithStream number _ _ _) =
      (number, object)
    createCouple object@(PDFXRefStream number _ _ _) = (number, object)
    createCouple object = (0, object)

insertObject :: PDFObject -> PDFObjects -> PDFObjects
insertObject object = case getObjectNumber object of
  Just number -> IM.insert number object
  Nothing     -> id

-- |
-- Find last value in a `CollectionOf` satisfying a predicate.
--
-- If the predicate is never satisfied, the function returns `Nothing`.
findLast :: (PDFObject -> Bool) -> PDFObjects -> Maybe PDFObject
findLast p = find p . fmap snd . IM.toDescList
