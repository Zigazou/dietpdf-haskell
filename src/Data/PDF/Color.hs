module Data.PDF.Color
  ( Color(ColorGray, ColorRGB, ColorCMYK, ColorGeneric)
  ) where

import Data.ByteString (ByteString)
import Data.Kind (Type)

type Color :: Type
data Color
  = ColorGray !Double
  | ColorRGB !Double !Double !Double
  | ColorCMYK !Double !Double !Double !Double
  | ColorGeneric ![Double] !(Maybe ByteString)
  deriving stock (Show, Eq)
