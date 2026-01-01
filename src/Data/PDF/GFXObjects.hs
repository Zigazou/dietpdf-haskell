{-|
A collection of `GFXObject`s stored in an array.
-}
module Data.PDF.GFXObjects (GFXObjects) where

import Data.Array (Array)
import Data.Kind (Type)
import Data.PDF.GFXObject (GFXObject)

{-|
A collection of `GFXObject`s stored in an array.
-}
type GFXObjects :: Type
type GFXObjects = Array GFXObject
