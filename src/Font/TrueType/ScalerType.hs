{-|
TrueType font scaler type identifiers.

Scaler types identify the font format and platform type within a TrueType font
file. Supports TrueType fonts, PostScript outlines (OpenType), and legacy
formats.
-}
module Font.TrueType.ScalerType
  ( ScalerType
    ( FontTrueTypeTrue
    , FontTrueType00010000
    , FontOldStyleSFNT
    , FontOpenType
    )
  , toScalerType
  , fromScalerType
  , isUnknown
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Kind (Type)

{-|
Font scaler type identifier.

Identifies the font format and platform type:

* 'FontTrueTypeTrue': Apple TrueType font ("true")
* 'FontTrueType00010000': Windows TrueType font ("\\000\\001\\000\\000")
* 'FontOldStyleSFNT': PostScript Type 1 font in SFNT wrapper ("typ1")
* 'FontOpenType': OpenType font with PostScript outlines ("OTTO")
* 'UnknownScalerType': Unrecognized format (for future compatibility)
-}
type ScalerType :: Type
data ScalerType
  = FontTrueTypeTrue -- ^ 'true' TrueType font
  | FontTrueType00010000 -- ^  '\000\001\000\000' TrueType font
  | FontOldStyleSFNT -- ^ 'typ1'
  | FontOpenType -- ^ 'OTTO'
  | UnknownScalerType ByteString -- ^ Unknown scaler type
  deriving stock (Eq, Show)

{-|
Test whether a scaler type is unknown.

Returns 'True' if the scaler type is 'UnknownScalerType', 'False' for any
recognized format.
-}
isUnknown :: ScalerType -> Bool
isUnknown (UnknownScalerType _) = True
isUnknown _anyOtherScalerType   = False

{-|
Convert a 4-byte identifier to a scaler type.

Maps the standard scaler type identifiers ("true", "\\000\\001\\000\\000",
"typ1", "OTTO") to their corresponding 'ScalerType' values. Unrecognized
identifiers are wrapped in 'UnknownScalerType'.
-}
toScalerType :: ByteString -> ScalerType
toScalerType "true"             = FontTrueTypeTrue
toScalerType "\000\001\000\000" = FontTrueType00010000
toScalerType "typ1"             = FontOldStyleSFNT
toScalerType "OTTO"             = FontOpenType
toScalerType identifier         = UnknownScalerType (BS.take 4 identifier)

{-|
Convert a scaler type back to its 4-byte identifier.

Inverse of 'toScalerType'. Returns the standard byte sequence for recognized
formats, or the original bytes for 'UnknownScalerType'.
-}
fromScalerType :: ScalerType -> ByteString
fromScalerType FontTrueTypeTrue               = "true"
fromScalerType FontTrueType00010000           = "\000\001\000\000"
fromScalerType FontOldStyleSFNT               = "typ1"
fromScalerType FontOpenType                   = "OTTO"
fromScalerType (UnknownScalerType identifier) = identifier
