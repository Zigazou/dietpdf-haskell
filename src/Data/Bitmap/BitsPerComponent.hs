module Data.Bitmap.BitsPerComponent
  ( BitsPerComponent (BC1Bit, BC2Bits, BC4Bits, BC8Bits, BC16Bits)
  , byteToWord16
  , bytesToWord16
  ) where

import Data.Bits (Bits (shiftL, shiftR, (.|.)), (.&.))
import Data.Kind (Type)
import Data.Word (Word16, Word8)

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

byteToWord16 :: BitsPerComponent -> Word8 -> [Word16]
byteToWord16 BC1Bit byte =
  [ if byte .&. 0x80 /= 0 then 1 else 0
  , if byte .&. 0x40 /= 0 then 1 else 0
  , if byte .&. 0x20 /= 0 then 1 else 0
  , if byte .&. 0x10 /= 0 then 1 else 0
  , if byte .&. 0x08 /= 0 then 1 else 0
  , if byte .&. 0x04 /= 0 then 1 else 0
  , if byte .&. 0x02 /= 0 then 1 else 0
  , if byte .&. 0x01 /= 0 then 1 else 0
  ]
byteToWord16 BC2Bits byte =
  [ fromIntegral ((byte .&. 0xC0) `shiftR` 6)
  , fromIntegral ((byte .&. 0x30) `shiftR` 4)
  , fromIntegral ((byte .&. 0x0C) `shiftR` 2)
  , fromIntegral (byte .&. 0x03)
  ]
byteToWord16 BC4Bits byte =
  [ fromIntegral ((byte .&. 0xF0) `shiftR` 4)
  , fromIntegral (byte .&. 0x0F)
  ]
byteToWord16 BC8Bits byte  = [ fromIntegral byte ]
byteToWord16 BC16Bits byte = [ fromIntegral byte ]

bytesToWord16 :: Word8 -> Word8 -> Word16
bytesToWord16 hi lo = (fromIntegral hi `shiftL` 8) .|. fromIntegral lo
