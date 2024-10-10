module Data.Statistics
  ( Statistics(pdfTotal, bitmapTotal, bitmapCount, vectorTotal, vectorCount, fontTotal, fontCount, fileTotal, fileCount, xmlTotal, xmlCount, otherTotal)
  , initStatistics
  ) where

import Data.Kind (Type)

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
