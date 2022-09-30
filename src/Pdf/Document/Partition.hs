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
  ) where

import           Data.Foldable                  ( find )
import           Data.Maybe                     ( fromMaybe )
import           Pdf.Document.Document          ( PDFDocument
                                                , singleton
                                                )
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFIndirectObject
                                                  , PDFIndirectObjectWithStream
                                                  , PDFNull
                                                  , PDFObjectStream
                                                  , PDFTrailer
                                                  , PDFVersion
                                                  )
                                                )

-- | A partition separates numbered objects from PDF versions and trailers.
data PDFPartition = PDFPartition
  { -- | Numbered objects in order of appearance
    ppIndirectObjects :: PDFDocument
  ,
    -- | PDF versions in order of appearance
    ppHeads           :: PDFDocument
  ,
    -- | Trailers in reverse order of appearance
    ppTrailers        :: PDFDocument
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
Partition a single PDF object.

Partition of a list of PDF objects is done using monoid.
-}
toPartition :: PDFObject -> PDFPartition
toPartition pno@PDFIndirectObject{} =
  PDFPartition (singleton pno) mempty mempty
toPartition pno@PDFIndirectObjectWithStream{} =
  PDFPartition (singleton pno) mempty mempty
toPartition pno@PDFObjectStream{} = PDFPartition (singleton pno) mempty mempty
toPartition ph@(PDFVersion _)     = PDFPartition mempty (singleton ph) mempty
toPartition pt@(PDFTrailer _)     = PDFPartition mempty mempty (singleton pt)
toPartition _                     = mempty

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
