module Pdf.Document.Stat
  ( stat
  , Statistics(..)
  , initStatistics
  ) where

import Control.Monad (foldM)

import Data.ByteString qualified as BS
import Data.Fallible (FallibleT)
import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.Logging (Logging)
import Data.Map qualified as Map

import Pdf.Document.Document (PDFDocument)
import Pdf.Document.Partition
    ( PDFPartition (ppObjectsWithStream)
    , partitionDocument
    )
import Pdf.Object.Object (PDFObject (PDFName))
import Pdf.Object.OptimizationType
    ( OptimizationType (GfxOptimization, JPGOptimization, TTFOptimization, XMLOptimization)
    , whatOptimizationFor
    )
import Pdf.Object.State (getDictionary, getStream)
import Pdf.Object.Unfilter (unfilter)

type Statistics :: Type
data Statistics = Statistics
  { pdfTotal    :: Int
  , bitmapTotal :: Int
  , bitmapCount :: Int
  , vectorTotal :: Int
  , vectorCount :: Int
  , fontTotal   :: Int
  , fontCount   :: Int
  , fileTotal   :: Int
  , fileCount   :: Int
  , xmlTotal    :: Int
  , xmlCount    :: Int
  , otherTotal  :: Int
  } deriving stock (Show)

type ObjectCategory :: Type
data ObjectCategory
  = Bitmap
  | Vector
  | Font
  | File
  | XML
  | Other
  deriving stock (Eq, Show)

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

objectCategory :: Logging IO => PDFObject -> FallibleT IO ObjectCategory
objectCategory object = do
    dict <- getDictionary object
    optimization <- unfilter object >>= whatOptimizationFor
    case optimization of
      XMLOptimization -> return XML
      GfxOptimization -> return Vector
      JPGOptimization -> return Bitmap
      TTFOptimization -> return Font
      _noOptimization -> case Map.lookup "Type" dict of
        (Just (PDFName "EmbeddedFile")) -> return File
        (Just (PDFName "XObject")) ->
          case Map.lookup "Subtype" dict of
            (Just (PDFName "Image"))        -> return Bitmap
            (Just (PDFName "Form"))         -> return Vector
            (Just (PDFName "Type1"))        -> return Font
            (Just (PDFName "EmbeddedFile")) -> return File
            (Just (PDFName "XML"))          -> return XML
            _anyOtherSubType                -> return Other
        (Just (PDFName "Font")) -> return Font
        (Just (PDFName "XML"))  -> return XML
        _anyOtherType           -> return Other

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
