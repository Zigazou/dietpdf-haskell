module Data.ObjectCategory
  ( ObjectCategory (Bitmap, Vector, Font, File, XML, Other)
  ) where

import Data.Kind (Type)

type ObjectCategory :: Type
data ObjectCategory
  = Bitmap
  | Vector
  | Font
  | File
  | XML
  | Other
  deriving stock (Eq, Show)
