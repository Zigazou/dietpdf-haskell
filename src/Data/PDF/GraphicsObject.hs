{-|
High-level categories of PDF graphics objects.

This module defines `GraphicsObject`, a coarse classification used to describe
which kind of graphics-related object is being handled (path construction,
text, clipping, shading, inline images, XObjects, etc.).
-}
module Data.PDF.GraphicsObject
  ( GraphicsObject (ContentStreamLevel, PathObject, TextObject, ClippingPathObject, ShadingObject, InLineImageObject, ExternalObject)
  )
where

import Data.Kind (Type)

{-|
Top-level graphics object category.

These constructors correspond to broad sections of the PDF content stream
model.
-}
type GraphicsObject :: Type
data GraphicsObject
  = {-| A whole content stream (top-level). -}
    ContentStreamLevel
  | {-| Path construction/painting operators. -}
    PathObject
  | {-| Text objects and text-showing operators. -}
    TextObject
  | {-| Clipping path related operators. -}
    ClippingPathObject
  | {-| Shading patterns / shading paint operators. -}
    ShadingObject
  | {-| Inline image objects (@BI@/@ID@/@EI@). -}
    InLineImageObject
  | {-| External objects (XObjects), e.g. invoked via @Do@. -}
    ExternalObject
  deriving stock (Eq, Show)
