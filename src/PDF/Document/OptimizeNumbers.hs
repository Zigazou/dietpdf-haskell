module PDF.Document.OptimizeNumbers
  ( optimizeNumbers
  ) where

import Data.Logging (Logging)
import Data.PDF.PDFObject (PDFObject (PDFNumber))
import Data.PDF.PDFWork (PDFWork)

import PDF.Processing.PDFWork (deepMapP, pModifyIndirectObjects)

import Util.Number (round')

reducePrecision :: Monad m => PDFObject -> PDFWork m PDFObject
reducePrecision (PDFNumber value) = return (PDFNumber $ round' 2 value)
reducePrecision anyOtherObject    = return anyOtherObject

optimizeNumbers :: Logging IO => PDFWork IO ()
optimizeNumbers = pModifyIndirectObjects (deepMapP reducePrecision)
