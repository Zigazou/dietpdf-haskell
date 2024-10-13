module Data.PDF.GFXObjects (GFXObjects) where

import Data.Array (Array)
import Data.Kind (Type)
import Data.PDF.GFXObject (GFXObject)

type GFXObjects :: Type
type GFXObjects = Array GFXObject
