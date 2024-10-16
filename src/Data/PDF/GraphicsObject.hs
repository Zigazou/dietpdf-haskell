module Data.PDF.GraphicsObject
  ( GraphicsObject (ContentStreamLevel, PathObject, TextObject, ClippingPathObject, ShadingObject, InLineImageObject, ExternalObject)
  )
where

import Data.Kind (Type)

type GraphicsObject :: Type
data GraphicsObject
  = ContentStreamLevel
  | PathObject
  | TextObject
  | ClippingPathObject
  | ShadingObject
  | InLineImageObject
  | ExternalObject
  deriving stock (Eq, Show)
