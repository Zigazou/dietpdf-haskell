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

type ScalerType :: Type
data ScalerType
  = FontTrueTypeTrue -- ^ 'true' TrueType font
  | FontTrueType00010000 -- ^  '\000\001\000\000' TrueType font
  | FontOldStyleSFNT -- ^ 'typ1'
  | FontOpenType -- ^ 'OTTO'
  | UnknownScalerType ByteString -- ^ Unknown scaler type
  deriving stock (Eq, Show)

isUnknown :: ScalerType -> Bool
isUnknown (UnknownScalerType _) = True
isUnknown _anyOtherScalerType   = False

toScalerType :: ByteString -> ScalerType
toScalerType "true"             = FontTrueTypeTrue
toScalerType "\000\001\000\000" = FontTrueType00010000
toScalerType "typ1"             = FontOldStyleSFNT
toScalerType "OTTO"             = FontOpenType
toScalerType identifier         = UnknownScalerType (BS.take 4 identifier)

fromScalerType :: ScalerType -> ByteString
fromScalerType FontTrueTypeTrue               = "true"
fromScalerType FontTrueType00010000           = "\000\001\000\000"
fromScalerType FontOldStyleSFNT               = "typ1"
fromScalerType FontOpenType                   = "OTTO"
fromScalerType (UnknownScalerType identifier) = identifier
