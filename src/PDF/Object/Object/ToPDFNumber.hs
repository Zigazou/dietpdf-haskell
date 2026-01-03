{-|
Type class for converting numeric types to PDF numbers

This module provides a type class that allows convenient conversion of various
numeric types (Int, Integer, Double) to PDF number objects. This enables
polymorphic creation of PDF numbers without explicit type annotations.
-}
module PDF.Object.Object.ToPDFNumber
  ( ToPDFNumber,
    mkPDFNumber,
  ) where

import Data.Kind (Constraint, Type)
import Data.PDF.PDFObject (PDFObject (PDFNumber))

{-|
Type class for converting numeric types to PDF number objects.

This type class provides a convenient way to create 'PDFNumber' objects from
various numeric types without requiring explicit conversions or type
annotations. Instances are provided for 'Double', 'Int', and 'Integer'.

Example usage:

@
mkPDFNumber 42       -- Creates PDFNumber 42.0 from Int
mkPDFNumber (3.14 :: Double)  -- Creates PDFNumber 3.14
mkPDFNumber (100 :: Integer)  -- Creates PDFNumber 100.0
@
-}
type ToPDFNumber :: Type -> Constraint
class ToPDFNumber t where
  {-|
  Create a PDF number object from a numeric value.

  Converts any numeric type in the 'ToPDFNumber' class to a 'PDFNumber' object.
  Internally, all numbers are stored as 'Double' in the PDF object.
  -}
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
