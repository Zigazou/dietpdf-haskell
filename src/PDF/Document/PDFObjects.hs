{- |
Helpers for collections of indirect PDF objects.

This module provides a small wrapper abstraction around collections of
'Data.PDF.PDFObject.PDFObject' values indexed by object number.

These helpers are primarily intended for encoding, optimizing, and otherwise
transforming PDF files, where it is convenient to treat the document as a map
from object numbers to objects.
-}
module PDF.Document.PDFObjects
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
import Data.PDF.PDFDocument (PDFDocument, fromList, toList)
import Data.PDF.PDFObject
    ( PDFObject (PDFIndirectObject, PDFIndirectObjectWithGraphics, PDFIndirectObjectWithStream, PDFXRefStream)
    )

{- |
A collection of PDF objects indexed by object number.

Internally this is an 'IntMap' keyed by the indirect object number.
-}
type PDFObjects :: Type
type PDFObjects = IM.IntMap PDFObject

{- |
Convert an object map back into a 'PDFDocument'.

Objects are emitted in ascending object-number order.
-}
toPDFDocument :: PDFObjects -> PDFDocument
toPDFDocument = fromList . fmap snd . IM.toAscList

{- |
Build an object map from a 'PDFDocument'.

The key is taken from the indirect object number for supported constructors.
Objects that are not indirect objects are currently stored under key @0@.
-}
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

{- |
Find the last object (by descending object number) satisfying a predicate.

Returns 'Nothing' if no object matches.
  -}
findLast :: (PDFObject -> Bool) -> PDFObjects -> Maybe PDFObject
findLast p = find p . fmap snd . IM.toDescList
