{-|
Color representations used in PDF graphics state.

This module defines a minimal 'Color' type covering common color spaces:
grayscale, RGB, CMYK, and a generic variant for color spaces with arbitrary
component counts. A sentinel 'ColorNotSet' represents an unset color.
-}
module Data.PDF.Color
  ( Color(ColorGray, ColorRGB, ColorCMYK, ColorGeneric, ColorNotSet)
  ) where

import Data.ByteString (ByteString)
import Data.Kind (Type)

{-|
PDF color value.

Constructors:

* 'ColorGray'  — single-component grayscale value.
* 'ColorRGB'   — red, green, blue components.
* 'ColorCMYK'  — cyan, magenta, yellow, key (black) components.
* 'ColorGeneric' — arbitrary component list with optional color space name
  ('ByteString') when available.
* 'ColorNotSet' — sentinel indicating no color has been set.
-}
type Color :: Type
data Color
  = ColorGray !Double
  | ColorRGB !Double !Double !Double
  | ColorCMYK !Double !Double !Double !Double
  | ColorGeneric ![Double] !(Maybe ByteString)
  | ColorNotSet
  deriving stock (Show, Eq)
