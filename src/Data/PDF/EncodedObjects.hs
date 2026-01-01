{-|
Collection of encoded PDF objects.

`EncodedObjects` is a map keyed by PDF object number, used to store
`EncodedObject`s with unique object numbers.
-}
module Data.PDF.EncodedObjects (EncodedObjects) where

import Data.IntMap.Strict (IntMap)
import Data.Kind (Type)

import Data.PDF.EncodedObject (EncodedObject)

{-|
A collection of encoded objects with no duplicates.

The key is the object number.
-}
type EncodedObjects :: Type
type EncodedObjects = IntMap EncodedObject
