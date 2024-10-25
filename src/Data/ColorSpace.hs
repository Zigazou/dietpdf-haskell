module Data.ColorSpace
  ( ColorSpace (ColorSpaceCMYK, ColorSpaceGray, ColorSpaceRGB, ColorSpaceUnknown)
  , csComponents
  , fromComponents
  , csTupleType
  ) where

import Data.ByteString (ByteString)
import Data.Kind (Type)

type ColorSpace :: Type
data ColorSpace = ColorSpaceRGB
                | ColorSpaceGray
                | ColorSpaceCMYK
                | ColorSpaceUnknown !Int
                deriving stock (Eq, Show)

csComponents :: ColorSpace -> Int
csComponents ColorSpaceGray                 = 1
csComponents ColorSpaceRGB                  = 3
csComponents ColorSpaceCMYK                 = 4
csComponents (ColorSpaceUnknown components) = components

fromComponents :: Int -> ColorSpace
fromComponents 1          = ColorSpaceGray
fromComponents 3          = ColorSpaceRGB
fromComponents 4          = ColorSpaceCMYK
fromComponents components = ColorSpaceUnknown components

csTupleType :: ColorSpace -> ByteString
csTupleType ColorSpaceGray = "GRAY"
csTupleType ColorSpaceRGB  = "RGB"
csTupleType ColorSpaceCMYK = "CMYK"
csTupleType _              = "UNKNOWN"
