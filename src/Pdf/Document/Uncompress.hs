module Pdf.Document.Uncompress
  ( uncompress
  ) where

import           Pdf.Document.Document          ( PDFDocument
                                                , cMap
                                                )
import           Pdf.Document.ObjectStream      ( explode )
import           Pdf.Object.Object              ( PDFObject
                                                , updateE
                                                )
import           Pdf.Object.Unfilter            ( unfilter )

uncompress :: PDFDocument -> PDFDocument
uncompress = cMap unfilter' . explode
 where
  unfilter' :: PDFObject -> PDFObject
  unfilter' object = case updateE object unfilter of
    Left  _          -> object
    Right unfiltered -> unfiltered
