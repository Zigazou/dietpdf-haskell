module Data.PDF.OptimizationType
  ( OptimizationType(XMLOptimization, GfxOptimization, ObjectStreamOptimization, XRefStreamOptimization, JPGOptimization, NoOptimization, TTFOptimization)
  )
where

import Data.Kind (Type)


type OptimizationType :: Type
data OptimizationType
    = XMLOptimization
    | GfxOptimization
    | ObjectStreamOptimization
    | XRefStreamOptimization
    | JPGOptimization
    | TTFOptimization
    | NoOptimization
    deriving stock (Eq)
