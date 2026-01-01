{-|
This module defines what is an array at the lowest level.
-}
module Data.Array
  ( Array
  , mkArray
  , mkEmptyArray
  , aFirst
  , aLast
  ) where

import Data.Kind (Type)
import Data.Sequence qualified as SQ

{-|
An `Array` is a `Sequence` of object.
-}
type Array :: Type -> Type
type Array a = SQ.Seq a

{-|
Create an empty `Array`.
-}
mkEmptyArray :: Array a
mkEmptyArray = SQ.empty

{-|
Create a `PDFArray` from a list of `PDFObject`.
-}
mkArray :: [a] -> Array a
mkArray = SQ.fromList


{-|
Get the first element of an `Array`, if it exists.
-}
aFirst :: Array a -> Maybe a
aFirst arr = case SQ.viewl arr of
  SQ.EmptyL -> Nothing
  x SQ.:< _ -> Just x

{-|
Get the last element of an `Array`, if it exists.
-}
aLast :: Array a -> Maybe a
aLast arr = case SQ.viewr arr of
  SQ.EmptyR -> Nothing
  _ SQ.:> x -> Just x
