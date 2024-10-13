module Data.PDF.EncodedObjects (EncodedObjects) where

import Data.IntMap.Strict (IntMap)
import Data.Kind (Type)

import Data.PDF.EncodedObject (EncodedObject)

-- | A collection of encoded objects with no duplicate
type EncodedObjects :: Type
type EncodedObjects = IntMap EncodedObject
