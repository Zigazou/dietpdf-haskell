{-|
Kinds of optimization passes supported by dietpdf.

This module defines an enumeration used to identify which optimization (if any)
is being requested or applied.

The constructors correspond to broad families of optimizations (graphics, XML,
object streams, etc.).
-}
module Data.PDF.OptimizationType
  ( OptimizationType(XMLOptimization, GfxOptimization, ObjectStreamOptimization, XRefStreamOptimization, JPGOptimization, NoOptimization, TTFOptimization)
  )
where

import Data.Kind (Type)

{-|
Identifier for an optimization pass/category.

This is typically used as a configuration or reporting value.
-}
type OptimizationType :: Type
data OptimizationType
  = XMLOptimization           -- ^ Optimize embedded XML payloads.
  | GfxOptimization           -- ^ Optimize graphics/content-stream operators.
  | ObjectStreamOptimization  -- ^ Optimize object stream usage/structure.
  | XRefStreamOptimization    -- ^ Optimize cross-reference stream usage/structure.
  | JPGOptimization           -- ^ Optimize JPEG images.
  | TTFOptimization           -- ^ Optimize embedded TrueType font programs.
  | NoOptimization            -- ^ Explicitly request no optimization.
    deriving stock (Eq)
