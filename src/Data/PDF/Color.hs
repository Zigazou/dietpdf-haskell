module Data.PDF.Color
  ( Color(ColorGray, ColorRGB, ColorCMYK, ColorGeneric, ColorNotSet)
  ) where

import Data.ByteString (ByteString)
import Data.Kind (Type)

type Color :: Type
data Color
  = ColorGray !Double
  | ColorRGB !Double !Double !Double
  | ColorCMYK !Double !Double !Double !Double
  | ColorGeneric ![Double] !(Maybe ByteString)
  | ColorNotSet
  deriving stock (Show, Eq)
