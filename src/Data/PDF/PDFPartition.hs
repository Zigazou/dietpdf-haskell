-- |
-- This modules allows partitioning of PDF objects.
--
-- Partitioning is used when encoded a whole PDF file from PDF objects.
module Data.PDF.PDFPartition
  ( PDFPartition(PDFPartition, ppHeads, ppObjectsWithStream, ppObjectsWithoutStream, ppTrailers)
  , lastTrailer
  , firstVersion
  ) where

import Data.Foldable (find)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.PDF.PDFDocument (PDFDocument)
import Data.PDF.PDFObject (PDFObject (PDFNull, PDFTrailer, PDFVersion))
import Data.PDF.PDFObjects (PDFObjects)


-- | A partition separates numbered objects from PDF versions and trailers.
type PDFPartition :: Type
data PDFPartition = PDFPartition
  { -- | Numbered objects with stream
    ppObjectsWithStream    :: !PDFObjects
  ,
    -- | Numbered objects without stream
    ppObjectsWithoutStream :: !PDFObjects
  ,
    -- | PDF versions in order of appearance
    ppHeads                :: !PDFDocument
  ,
    -- | Trailers in reverse order of appearance
    ppTrailers             :: !PDFDocument
  }
  deriving stock (Eq, Show)

instance Semigroup PDFPartition where
  (<>) :: PDFPartition -> PDFPartition -> PDFPartition
  (<>) (PDFPartition m1 n1 h1 t1) (PDFPartition m2 n2 h2 t2) =
    PDFPartition (m1 <> m2) (n1 <> n2) (h1 <> h2) (t2 <> t1)

instance Monoid PDFPartition where
  mempty :: PDFPartition
  mempty = PDFPartition mempty mempty mempty mempty

{-|
Return the last trailer if any.

If the partition does not have a trailer, it returns an empty trailer.
-}
lastTrailer :: PDFPartition -> PDFObject
lastTrailer = fromMaybe (PDFTrailer PDFNull) . find trailer . ppTrailers
 where
  trailer :: PDFObject -> Bool
  trailer (PDFTrailer _) = True
  trailer _              = False

{-|
Return the first PDF version if any.

If the partition does not have a PDF version, it returns a default PDF
version of 1.0.
-}
firstVersion :: PDFPartition -> PDFObject
firstVersion = fromMaybe (PDFVersion "1.0") . find version . ppHeads
 where
  version :: PDFObject -> Bool
  version (PDFVersion _) = True
  version _              = False
