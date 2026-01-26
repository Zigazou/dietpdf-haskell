module Data.Bitmap.BitsPerComponent
  ( BitsPerComponent (BC1Bit, BC2Bits, BC4Bits, BC8Bits, BC16Bits)
  ) where

import Data.Kind (Type)

type BitsPerComponent :: Type
data BitsPerComponent
  = BC1Bit -- ^ 1 bit per component
  | BC2Bits -- ^ 2 bits per component
  | BC4Bits -- ^ 4 bits per component
  | BC8Bits -- ^ 8 bits per component
  | BC16Bits -- ^ 16 bits per component
  deriving stock (Eq, Show)

instance Enum BitsPerComponent where
  toEnum :: Int -> BitsPerComponent
  toEnum 1             = BC1Bit
  toEnum 2             = BC2Bits
  toEnum 4             = BC4Bits
  toEnum 8             = BC8Bits
  toEnum 16            = BC16Bits
  toEnum _invalidValue = error ("BitsPerComponent.toEnum: invalid value: "
                                ++ show _invalidValue)

  fromEnum :: BitsPerComponent -> Int
  fromEnum BC1Bit   = 1
  fromEnum BC2Bits  = 2
  fromEnum BC4Bits  = 4
  fromEnum BC8Bits  = 8
  fromEnum BC16Bits = 16
