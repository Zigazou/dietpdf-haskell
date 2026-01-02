{-|
Computes and displays per-document statistics across multiple PDF files.

This module exposes 'showStat', which prints a header row and then one line per
input document summarizing counts and sizes for bitmaps, vectors, fonts,
embedded files, XML content, and totals. It uses 'stat' to accumulate values
starting from 'initStatistics', including the provided document size.
-}
module Command.Stat
  ( showStat
  ) where

import Control.Monad (forM_)

import Data.Fallible (FallibleT)
import Data.PDF.PDFDocument (PDFDocument)
import Data.PDF.PDFWork (evalPDFWork)
import Data.PDF.Statistics
    ( Statistics (bitmapCount, bitmapTotal, fileCount, fileTotal, fontCount, fontTotal, otherTotal, pdfTotal, vectorCount, vectorTotal, xmlCount, xmlTotal)
    , initStatistics
    )

import PDF.Document.Stat (stat)

import Util.Display (disp)

{-|
Column headers for the statistics table.

Columns include per-category counts and sizes, structure size (computed as
PDF total minus category subtotals), other size, and overall total size.
-}
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

{-|
Print aggregated statistics for a list of documents.

Input: a list of tuples @(filename, documentSize, document)@ where
'documentSize' is the raw size in bytes and 'document' is the parsed
'PDFDocument'.

Behavior:

* Prints the header row via 'disp'.
* For each document, evaluates 'stat' with an initial 'Statistics' seeded by
  'initStatistics documentSize'.
* Computes the structures size as the PDF total minus the sum of category
  subtotals.
* Prints a formatted row for the document with counts and sizes for each
  category and totals.

Side effects: writes rows to stdout via 'disp'.
-}
showStat :: [(FilePath, Int, PDFDocument)] -> FallibleT IO ()
showStat documents = do
  disp header

  forM_ documents $ \(filename, documentSize, document) -> do
    statistics <- evalPDFWork (stat document (initStatistics documentSize))

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
