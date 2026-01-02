{-|
Parse TrueType scaler type identifiers.

The scaler type is a 4-byte value that identifies the font format and platform
type (e.g., 0x00010000 for TrueType, 0x4F54544F for PostScript/CFF, etc.).
-}
module Font.TrueType.Parser.ScalerType
  ( scalerTypeP
  ) where

import Data.Binary.Get.Internal (getByteString)
import Data.Binary.Parser (Get)

import Font.TrueType.ScalerType (ScalerType, toScalerType)

{-|
Parse a 4-byte scaler type identifier.

Reads exactly 4 bytes and converts them to a 'ScalerType' value via
'toScalerType'. The scaler type determines the font outline format (TrueType,
PostScript, Windows, etc.).
-}
scalerTypeP :: Get ScalerType
scalerTypeP = toScalerType <$> getByteString 4
