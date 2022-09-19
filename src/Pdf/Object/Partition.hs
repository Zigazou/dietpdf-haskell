{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- |
-- This modules allows partitioning of PDF objects.
--
-- Partitioning is used when encoded a whole PDF file from PDF objects.
module Pdf.Object.Partition
  ( PDFPartition(PDFPartition, ppIndirectObjects, ppHeads, ppTrailers)
  , toPartition
  , firstVersion
  , lastTrailer
  ) where

import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFVersion
                                                  , PDFTrailer
                                                  , PDFIndirectObject
                                                  , PDFNull
                                                  )
                                                )

-- | A partition separates numbered objects from PDF versions and trailers.
data PDFPartition = PDFPartition
  { -- | Numbered objects in order of appearance
    ppIndirectObjects :: [PDFObject]
  ,
    -- | PDF versions in order of appearance
    ppHeads           :: [PDFObject]
  ,
    -- | Trailers in reverse order of appearance
    ppTrailers        :: [PDFObject]
  }
  deriving stock (Eq, Show)

instance Semigroup PDFPartition where
  (<>) (PDFPartition n1 h1 t1) (PDFPartition n2 h2 t2) =
    PDFPartition (n1 ++ n2) (h1 ++ h2) (t2 ++ t1)

instance Monoid PDFPartition where
  mempty = PDFPartition [] [] []

{-|
Partition a single PDF object.

Partition of a list of PDF objects is done using monoid.
-}
toPartition :: PDFObject -> PDFPartition
toPartition pno@PDFIndirectObject{} = PDFPartition [pno] [] []
toPartition ph@(PDFVersion _)       = PDFPartition [] [ph] []
toPartition pt@(PDFTrailer _)       = PDFPartition [] [] [pt]
toPartition _                       = mempty

{-|
Return the first PDF version if any.

If the partition does not have a PDF version, it returns a default PDF
version of 1.0.
-}
firstVersion :: PDFPartition -> PDFObject
firstVersion (PDFPartition _ (ph@(PDFVersion _) : _) _) = ph
firstVersion _anyOtherValue                             = PDFVersion "PDF-1.0"

{-|
Return the last trailer if any.

If the partition does not have a trailer, it returns an empty trailer.
-}
lastTrailer :: PDFPartition -> PDFObject
lastTrailer (PDFPartition _ _ (pt@(PDFTrailer _) : _)) = pt
lastTrailer _anyOtherValue                             = PDFTrailer PDFNull
