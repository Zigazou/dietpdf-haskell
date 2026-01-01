{- |
Collect soft-mask references from a PDF document.

This module extracts the object numbers referenced by @/SMask@ entries in PDF
objects. These references are used for transparency soft masks in PDF graphics.

The main entry point 'getAllMasks' traverses a whole 'PDFDocument' and returns
the set of referenced mask object numbers.
-}
module PDF.Document.GetAllMasks
  ( getAllMasks
  )
where

import Data.PDF.PDFDocument (PDFDocument, deepFind, toList)
import Data.PDF.PDFObject (PDFObject (PDFReference))
import Data.Set (Set)
import Data.Set qualified as Set

import PDF.Object.Object.Properties (getValueForKey)

{- |
Predicate used to detect objects that carry a soft-mask reference.

Returns 'True' when the object has an @/SMask@ key.
-}
isMaskReference :: PDFObject -> Bool
isMaskReference object = case getValueForKey "SMask" object of
  Just _anyValue -> True
  _anyOtherCase  -> False

{- |
Extract the referenced soft-mask object number from an object.

If the @/SMask@ entry is a 'PDFReference', its major number is returned.
Otherwise (or if no @/SMask@ exists), this returns @0@.
-}
getNumber :: PDFObject -> Int
getNumber object = case getValueForKey "SMask" object of
  Just (PDFReference major _minor) -> major
  _anyOtherCase                    -> 0

{- |
Collect all soft-mask object numbers referenced by @/SMask@ entries.

The result is a set of major object numbers. The placeholder value @0@ is
filtered out.
-}
getAllMasks :: PDFDocument -> Set Int
getAllMasks = Set.delete 0
            . Set.fromList
            . fmap getNumber
            . toList
            . deepFind isMaskReference
