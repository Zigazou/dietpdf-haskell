module Command.Stat
  ( showStat
  ) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)

import Data.Fallible (FallibleT)

import Pdf.Document.Document (PDFDocument)
import Pdf.Document.Stat
    ( Statistics (bitmapCount, bitmapTotal, fileCount, fileTotal, fontCount, fontTotal, otherTotal, pdfTotal, vectorCount, vectorTotal, xmlCount, xmlTotal)
    , initStatistics
    , stat
    )

showStat :: [(FilePath, Int, PDFDocument)] -> FallibleT IO ()
showStat documents = do
  disp "pdf\t\
       \bitmaps count\tbitmaps size\t\
       \vectors count\tvectors size\t\
       \fonts count\tfonts size\t\
       \files count\tfiles size\t\
       \xml count\txml size\t\
       \structures size\t\
       \others size\t\
       \total"

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
      $ filename ++ "\t"
     ++ show (bitmapCount statistics) ++ "\t" ++ show (bitmapTotal statistics) ++ "\t"
     ++ show (vectorCount statistics) ++ "\t" ++ show (vectorTotal statistics) ++ "\t"
     ++ show (fontCount statistics) ++ "\t" ++ show (fontTotal statistics) ++ "\t"
     ++ show (fileCount statistics) ++ "\t" ++ show (fileTotal statistics) ++ "\t"
     ++ show (xmlCount statistics) ++ "\t" ++ show (xmlTotal statistics) ++ "\t"
     ++ show (pdfTotal statistics - subtotals) ++ "\t"
     ++ show (otherTotal statistics) ++ "\t"
     ++ show (pdfTotal statistics)
  where
    disp :: String -> FallibleT IO ()
    disp = liftIO . putStrLn