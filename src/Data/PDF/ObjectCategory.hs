{-|
High-level classification of PDF objects.

This module defines a small, coarse-grained taxonomy used by dietpdf to
categorize objects found in a PDF (typically for reporting/statistics).

The categories are intentionally broad and may not correspond 1:1 with PDF
object types; they are a convenience layer for higher-level code.
-}
module Data.PDF.ObjectCategory
  ( ObjectCategory (Bitmap, Vector, Font, File, XML, Other)
  ) where

import Data.Kind (Type)

{-|
High-level category assigned to a PDF object.

This is used to group objects into buckets (images, vector content, fonts,
embedded files, etc.).
-}
type ObjectCategory :: Type
data ObjectCategory
  = Bitmap -- ^ Bitmap image data (for example, raster images).
  | Vector -- ^ Vector graphics content.
  | Font   -- ^ Font program or font-related resources.
  | File   -- ^ Embedded file data.
  | XML    -- ^ XML payloads (for example, XFA/XMP-like structures).
  | Other  -- ^ Anything that does not fit the other categories.
  deriving stock (Eq, Show)
