module Command.Stat
  ( showStat
  ) where

import Control.Monad (forM_)

import Data.Fallible (FallibleT)
import Data.Statistics
    ( Statistics (bitmapCount, bitmapTotal, fileCount, fileTotal, fontCount, fontTotal, otherTotal, pdfTotal, vectorCount, vectorTotal, xmlCount, xmlTotal)
    , initStatistics
    )

import Pdf.Document.Document (PDFDocument)
import Pdf.Document.Stat (stat)

import Util.Display (disp)

header :: [String]
header = [ "pdf"
         , "bitmaps count", "bitmaps size"
         , "vectors count", "vectors size"
         , "fonts count", "fonts size"
         , "files count", "files size"
         , "xml count", "xml size"
         , "structures size"
         , "others size"
         , "total"
         ]

showStat :: [(FilePath, Int, PDFDocument)] -> FallibleT IO ()
showStat documents = do
  disp header

  forM_ documents $ \(filename, documentSize, document) -> do
    statistics <- stat document (initStatistics documentSize)

    let subtotals = sum ([ bitmapTotal
                         , vectorTotal
                         , fontTotal
                         , fileTotal
                         , xmlTotal
                         , otherTotal
                         ] <*> [statistics]
                        )

    disp
      [ filename
      , show (bitmapCount statistics), show (bitmapTotal statistics)
      , show (vectorCount statistics), show (vectorTotal statistics)
      , show (fontCount statistics), show (fontTotal statistics)
      , show (fileCount statistics), show (fileTotal statistics)
      , show (xmlCount statistics), show (xmlTotal statistics)
      , show (pdfTotal statistics - subtotals)
      , show (otherTotal statistics)
      , show (pdfTotal statistics)
      ]
