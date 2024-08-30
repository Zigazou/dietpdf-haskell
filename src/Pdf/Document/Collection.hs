-- |
-- This module manipulates collections of `PDFObject`.
--
-- These functions are meant to be used when optimizing/encoding a PDF.
module Pdf.Document.Collection
  ( -- * Encoding of object collections
    PDFObjects,
    toPDFDocument,
    fromPDFDocument,
    findLast,
  )
where

import Data.IntMap.Strict qualified as IM
import Data.Kind (Type)
import Data.List (find)
import Pdf.Document.Document (PDFDocument, fromList, toList)
import Pdf.Object.Object
  ( PDFObject (PDFIndirectObject, PDFIndirectObjectWithGraphics, PDFIndirectObjectWithStream, PDFXRefStream),
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

-- |
-- Find last value in a `CollectionOf` satisfying a predicate.
--
-- If the predicate is never satisfied, the function returns `Nothing`.
findLast :: (PDFObject -> Bool) -> PDFObjects -> Maybe PDFObject
findLast p = find p . fmap snd . IM.toDescList
