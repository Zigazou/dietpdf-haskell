{-|
This module groups all the errors that DietPDF may generate.

Having one type for all errors means the Either monad can be used to avoid
long if then else if then else.
-}
module Data.ErrorType ( ErrorType(..) ) where

import Data.Kind (Type)

type ErrorType :: Type
data ErrorType = ReadingError
               | ParsingError
               | EncodingError
               | StructureError
               | UnsupportedError
               deriving stock Eq

instance Show ErrorType where
  show :: ErrorType -> String
  show ReadingError     = "reading"
  show ParsingError     = "parsing"
  show EncodingError    = "encoding"
  show StructureError   = "structure"
  show UnsupportedError = "unsupported"
