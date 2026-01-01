{-|
Simple counters for categorized PDF object totals.

This module defines a lightweight record used to track byte totals and counts
for different object categories (bitmap images, vector content, fonts, files,
XML, and an "other" bucket), along with the total size of the PDF.
-}
module Data.PDF.Statistics
  ( Statistics(pdfTotal, bitmapTotal, bitmapCount, vectorTotal, vectorCount, fontTotal, fontCount, fileTotal, fileCount, xmlTotal, xmlCount, otherTotal)
  , initStatistics
  ) where

import Data.Kind (Type)

{-|
Structured counters for reporting.

Fields ending in @Total@ represent accumulated sizes (bytes). Fields ending in
@Count@ represent the number of objects observed in that category.
-}
type Statistics :: Type
data Statistics = Statistics
  { pdfTotal    :: Int -- ^ Total size of the PDF file (bytes).
  , bitmapTotal :: Int -- ^ Total size of bitmap image data (bytes).
  , bitmapCount :: Int -- ^ Number of bitmap image objects.
  , vectorTotal :: Int -- ^ Total size of vector graphics content (bytes).
  , vectorCount :: Int -- ^ Number of vector graphics objects.
  , fontTotal   :: Int -- ^ Total size of font program data (bytes).
  , fontCount   :: Int -- ^ Number of font objects.
  , fileTotal   :: Int -- ^ Total size of embedded file data (bytes).
  , fileCount   :: Int -- ^ Number of embedded file objects.
  , xmlTotal    :: Int -- ^ Total size of XML payloads (bytes).
  , xmlCount    :: Int -- ^ Number of XML objects.
  , otherTotal  :: Int -- ^ Total size of other objects (bytes).
  } deriving stock (Show)

{-|
Initialize a 'Statistics' record with the total PDF size.

All category counters are set to zero.
-}
initStatistics :: Int -> Statistics
initStatistics total = Statistics
  { pdfTotal    = total
  , bitmapTotal = 0
  , bitmapCount = 0
  , vectorTotal = 0
  , vectorCount = 0
  , fontTotal   = 0
  , fontCount   = 0
  , fileTotal   = 0
  , fileCount   = 0
  , xmlTotal    = 0
  , xmlCount    = 0
  , otherTotal  = 0
  }
