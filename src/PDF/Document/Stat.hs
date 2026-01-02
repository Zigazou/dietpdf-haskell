{-|
Compute PDF document statistics.

Provides utilities for analyzing PDF objects by category (bitmaps, vectors,
fonts, files, XML, other) and computing size statistics for each category type.
-}
module PDF.Document.Stat
  ( stat
  ) where

import Control.Monad (foldM)

import Data.ByteString qualified as BS
import Data.Functor ((<&>))
import Data.Logging (Logging)
import Data.PDF.ObjectCategory
    ( ObjectCategory (Bitmap, File, Font, Other, Vector, XML)
    )
import Data.PDF.PDFDocument (PDFDocument)
import Data.PDF.PDFObject (PDFObject)
import Data.PDF.PDFPartition (PDFPartition (ppObjectsWithStream))
import Data.PDF.PDFWork (PDFWork)
import Data.PDF.Statistics
    ( Statistics (bitmapCount, bitmapTotal, fileCount, fileTotal, fontCount, fontTotal, otherTotal, vectorCount, vectorTotal, xmlCount, xmlTotal)
    )

import PDF.Document.PDFPartition (partitionDocument)
import PDF.Object.State (getStream)
import PDF.Processing.ObjectCategory (objectCategory)

{-|
Update statistics with a single PDF object.

Extracts the stream content, determines the object's category (bitmap, vector,
font, file, XML, or other), and increments the corresponding counters and size
totals in the statistics record.
-}
updateStatistics
  :: Logging IO
  => Statistics
  -> PDFObject
  -> PDFWork IO Statistics
updateStatistics statistics object = do
  objectSize <- getStream object <&> BS.length

  objectCategory object >>= \case
    Bitmap -> return statistics
      { bitmapTotal = bitmapTotal statistics + objectSize
      , bitmapCount = bitmapCount statistics + 1
      }

    Vector -> return statistics
      { vectorTotal = vectorTotal statistics + objectSize
      , vectorCount = vectorCount statistics + 1
      }

    Font -> return statistics
      { fontTotal = fontTotal statistics + objectSize
      , fontCount = fontCount statistics + 1
      }

    File -> return statistics
      { fileTotal = fileTotal statistics + objectSize
      , fileCount = fileCount statistics + 1
      }

    XML -> return statistics
      { xmlTotal = xmlTotal statistics + objectSize
      , xmlCount = xmlCount statistics + 1
      }

    Other -> return statistics
      { otherTotal = otherTotal statistics + objectSize }

{-|
Compute comprehensive statistics for a PDF document.

Partitions the document and iterates through all objects with streams,
categorizing each and accumulating statistics. Returns a 'Statistics' record
containing counts and total sizes for each object category.
-}
stat
  :: Logging IO
  => PDFDocument
  -> Statistics
  -> PDFWork IO Statistics
stat objects statistics = do
  let partition = partitionDocument objects
  foldM updateStatistics statistics (ppObjectsWithStream partition)
