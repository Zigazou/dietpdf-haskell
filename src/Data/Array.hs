{-|
This module defines what is an array at the lowest level.
-}
module Data.Array
  ( Array
  , mkArray
  ) where

import Data.Kind (Type)
import Data.Sequence qualified as SQ

{-|
An `Array` is a `Sequence` of object.
-}
type Array :: Type -> Type
type Array a = SQ.Seq a

{-|
Create a `PDFArray` from a list of `PDFObject`.
-}
mkArray :: [a] -> Array a
mkArray = SQ.fromList
