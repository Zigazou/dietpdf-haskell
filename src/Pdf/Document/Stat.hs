module Pdf.Document.Stat
  ( stat
  ) where

import Control.Monad (foldM)

import Data.ByteString qualified as BS
import Data.Fallible (FallibleT)
import Data.Functor ((<&>))
import Data.Logging (Logging)
import Data.ObjectCategory
    ( ObjectCategory (Bitmap, File, Font, Other, Vector, XML)
    )
import Data.Statistics
    ( Statistics (bitmapCount, bitmapTotal, fileCount, fileTotal, fontCount, fontTotal, otherTotal, vectorCount, vectorTotal, xmlCount, xmlTotal)
    )

import Pdf.Document.Document (PDFDocument)
import Pdf.Document.Partition
    ( PDFPartition (ppObjectsWithStream)
    , partitionDocument
    )
import Pdf.Object.Object (PDFObject)
import Pdf.Object.ObjectCategory (objectCategory)
import Pdf.Object.State (getStream)

updateStatistics
  :: Logging IO
  => Statistics
  -> PDFObject
  -> FallibleT IO Statistics
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

stat
  :: Logging IO
  => PDFDocument
  -> Statistics
  -> FallibleT IO Statistics
stat objects statistics = do
  let partition = partitionDocument objects
  foldM updateStatistics statistics (ppObjectsWithStream partition)
