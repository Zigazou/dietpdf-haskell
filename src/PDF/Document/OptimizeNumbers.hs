{-|
Optimize numeric values in PDF documents.

Provides utilities for reducing the precision of floating-point numbers
throughout a PDF document to decrease file size while maintaining visual quality.
-}
module PDF.Document.OptimizeNumbers
  ( optimizeNumbers
  ) where

import Data.PDF.PDFObject (PDFObject (PDFNumber))
import Data.PDF.PDFWork (PDFWork)

import PDF.Processing.PDFWork (deepMapP, pModifyIndirectObjects)

import Util.Number (round')

{-|
Reduce the precision of numeric values.

Rounds floating-point numbers to 4 decimal places. Non-numeric objects
are returned unchanged.
-}
reducePrecision :: Monad m => PDFObject -> PDFWork m PDFObject
reducePrecision (PDFNumber value) = return (PDFNumber $ round' 4 value)
reducePrecision anyOtherObject    = return anyOtherObject

{-|
Optimize all numeric values in a PDF document.

Traverses all indirect objects in the document and reduces the precision of
all floating-point numbers to 4 decimal places. This optimization reduces
file size while preserving visual fidelity for most PDF content.
-}
optimizeNumbers :: PDFWork IO ()
optimizeNumbers = pModifyIndirectObjects (deepMapP reducePrecision)
