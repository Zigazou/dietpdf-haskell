{-|
Simple color space enumeration and helpers.

Represents a small set of common color spaces (RGB, Gray, CMYK) plus an
unknown variant carrying an explicit component count. Utilities convert between
component counts and 'ColorSpace' values and provide a tuple-type tag for
downstream formatting.
-}
module Data.ColorSpace
  ( ColorSpace (ColorSpaceCMYK, ColorSpaceGray, ColorSpaceRGB, ColorSpaceUnknown)
  , csComponents
  , fromComponents
  , csTupleType
  ) where

import Data.ByteString (ByteString)
import Data.Kind (Type)

{-|
Enumeration of supported color spaces.

Constructors:

* 'ColorSpaceRGB'  — red, green, blue (3 components).
* 'ColorSpaceGray' — grayscale (1 component).
* 'ColorSpaceCMYK' — cyan, magenta, yellow, key/black (4 components).
* 'ColorSpaceUnknown' — arbitrary component count for unsupported spaces.
-}
type ColorSpace :: Type
data ColorSpace = ColorSpaceRGB
                | ColorSpaceGray
                | ColorSpaceCMYK
                | ColorSpaceUnknown !Int
                deriving stock (Eq, Show)

{-|
Number of components associated with a 'ColorSpace'.
-}
csComponents :: ColorSpace -> Int
csComponents ColorSpaceGray                 = 1
csComponents ColorSpaceRGB                  = 3
csComponents ColorSpaceCMYK                 = 4
csComponents (ColorSpaceUnknown components) = components

{-|
Build a 'ColorSpace' from a component count.

Recognizes 1→Gray, 3→RGB, 4→CMYK; otherwise produces 'ColorSpaceUnknown'.
-}
fromComponents :: Int -> ColorSpace
fromComponents 1          = ColorSpaceGray
fromComponents 3          = ColorSpaceRGB
fromComponents 4          = ColorSpaceCMYK
fromComponents components = ColorSpaceUnknown components

{-|
Short tag name for a 'ColorSpace', used in tuple formatting.

Returns "GRAY", "RGB", "CMYK", or "UNKNOWN".
-}
csTupleType :: ColorSpace -> ByteString
csTupleType ColorSpaceGray = "GRAY"
csTupleType ColorSpaceRGB  = "RGB"
csTupleType ColorSpaceCMYK = "CMYK"
csTupleType _              = "UNKNOWN"
