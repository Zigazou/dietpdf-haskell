module Pdf.Document.Uncompress
  ( uncompress
  ) where

import           Pdf.Document.Document          ( PDFDocument
                                                , cMap
                                                )
import           Pdf.Document.ObjectStream      ( explode )
import           Pdf.Object.Object              ( PDFObject )
import           Pdf.Object.State               ( updateE )
import           Pdf.Object.Unfilter            ( unfilter )

{- |
Uncompress all `PDFObject` contained in a `PDFDocument`.

Objects embedded in object streams are extracted and the object stream is
removed.

If a `PDFObject` cannot be uncompressed (meaning its processing generated an
error), the object is left as is. Thus this function may leave object
uncompressed.
-}
uncompress :: PDFDocument -> PDFDocument
uncompress = cMap unfilter' . explode
 where
  unfilter' :: PDFObject -> PDFObject
  unfilter' object = case updateE object unfilter of
    Left  _          -> object
    Right unfiltered -> unfiltered
