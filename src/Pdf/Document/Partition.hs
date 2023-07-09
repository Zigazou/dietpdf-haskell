{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE InstanceSigs #-}

-- |
-- This modules allows partitioning of PDF objects.
--
-- Partitioning is used when encoded a whole PDF file from PDF objects.
module Pdf.Document.Partition
  ( PDFPartition(PDFPartition, ppHeads, ppIndirectObjects, ppTrailers)
  , toPartition
  , firstVersion
  , lastTrailer
  , removeUnused
  ) where

import           Data.Foldable                  ( find )
import           Data.Maybe                     ( fromMaybe )
import           Pdf.Document.Document          ( PDFDocument
                                                , cFilter
                                                , deepFind
                                                )
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFNull
                                                  , PDFTrailer
                                                  , PDFVersion
                                                  , PDFIndirectObject
                                                  , PDFIndirectObjectWithStream
                                                  , PDFObjectStream
                                                  , PDFReference
                                                  )
                                                , isIndirect
                                                , isHeader
                                                , isTrailer
                                                , hasKey
                                                )
import           Util.Logging                   ( Logging, sayF )
import           Util.UnifiedError              ( FallibleT )
import           Pdf.Document.Uncompress        ( uncompress )

-- | A partition separates numbered objects from PDF versions and trailers.
data PDFPartition = PDFPartition
  { -- | Numbered objects in order of appearance
    ppIndirectObjects :: !PDFDocument
  ,
    -- | PDF versions in order of appearance
    ppHeads           :: !PDFDocument
  ,
    -- | Trailers in reverse order of appearance
    ppTrailers        :: !PDFDocument
  }
  deriving stock (Eq, Show)

instance Semigroup PDFPartition where
  (<>) :: PDFPartition -> PDFPartition -> PDFPartition
  (<>) (PDFPartition n1 h1 t1) (PDFPartition n2 h2 t2) =
    PDFPartition (n1 <> n2) (h1 <> h2) (t2 <> t1)

instance Monoid PDFPartition where
  mempty :: PDFPartition
  mempty = PDFPartition mempty mempty mempty

{-|
Partition a `CollectionOf` of `PDFObject`.
-}
toPartition :: PDFDocument -> PDFPartition
toPartition objects = PDFPartition
  { ppIndirectObjects = cFilter isIndirect objects
  , ppHeads           = cFilter isHeader objects
  , ppTrailers        = cFilter isTrailer objects
  }

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

removeUnused :: Logging m => PDFPartition -> FallibleT m PDFPartition
removeUnused (PDFPartition indirectObjects heads trailers) = do
  sayF "  - Uncompressing indirect objects"
  uIndirectObjects <- uncompress indirectObjects
  sayF "  - Uncompressing head objects"
  uHeads           <- uncompress heads
  sayF "  - Uncompressing trailer objects"
  uTrailers        <- uncompress trailers

  sayF "  - Locating all references"
  let references = deepFind isReference uHeads
                <> deepFind isReference uTrailers
                <> deepFind isReference uIndirectObjects

  sayF "  - Removing unused objects"

  return $ PDFPartition
    { ppIndirectObjects = cFilter (used references) indirectObjects
    , ppHeads           = heads
    , ppTrailers        = trailers
    }
 where
  isNotLinearized :: PDFObject -> Bool
  isNotLinearized = not . hasKey "Linearized"

  isReferenced :: PDFDocument -> PDFObject -> Bool
  isReferenced refs (PDFIndirectObject num gen _) =
    PDFReference num gen `elem` refs
  isReferenced refs (PDFIndirectObjectWithStream num gen _ _) =
    PDFReference num gen `elem` refs
  isReferenced refs (PDFObjectStream num gen _ _) =
    PDFReference num gen `elem` refs
  isReferenced _anyRefs _anyOtherObject = True

  used :: PDFDocument -> PDFObject -> Bool
  used refs object = isNotLinearized object && isReferenced refs object

  isReference :: PDFObject -> Bool
  isReference PDFReference{}  = True
  isReference _anyOtherObject = False
