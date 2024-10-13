{-|
This module defines what is a PDF object and functions in relation with the
PDF specification.
-}
module Pdf.Object.Object.ToPDFNumber
  ( ToPDFNumber,
    mkPDFNumber,
  ) where

import Data.Kind (Constraint, Type)
import Data.PDF.PDFObject (PDFObject (PDFNumber))

{- |
Allow easy creation of `PDFNumber` objects with auto conversion.
-}
type ToPDFNumber :: Type -> Constraint
class ToPDFNumber t where
  mkPDFNumber :: t -> PDFObject

instance ToPDFNumber Double where
  mkPDFNumber :: Double -> PDFObject
  mkPDFNumber = PDFNumber

instance ToPDFNumber Int where
  mkPDFNumber :: Int -> PDFObject
  mkPDFNumber = PDFNumber . fromIntegral

instance ToPDFNumber Integer where
  mkPDFNumber :: Integer -> PDFObject
  mkPDFNumber = PDFNumber . fromIntegral
