module Data.Bitmap.BitmapConfiguration
  ( BitmapConfiguration (BitmapConfiguration, bcLineWidth, bcComponents, bcBitsPerComponent)
  , bitmapRawWidth
  , findBitmapConfigurations
  ) where

import Data.Bitmap.BitsPerComponent
  (BitsPerComponent (BC16Bits, BC2Bits, BC4Bits, BC8Bits))
import Data.Bits ((.&.))
import Data.Kind (Type)

{-|
Module for bitmap configuration used in predictors that may change
-}
type BitmapConfiguration :: Type
data BitmapConfiguration = BitmapConfiguration
  { bcLineWidth        :: !Int -- ^ Width in pixels
  , bcComponents       :: !Int -- ^ Number of color components per pixel
  , bcBitsPerComponent :: !BitsPerComponent -- ^ Bits per color component
  }
  deriving stock (Eq, Show)

{-|
Calculate the raw width in bytes of a bitmap line given its configuration.

Depending on the bits per component, the line may not be byte-aligned, this
function takes that into account and rounds up to the nearest byte.
-}
bitmapRawWidth :: BitmapConfiguration -> Int
bitmapRawWidth (BitmapConfiguration lineWidth components bitsPerComponent) =
    div (totalBits + missingBits) 8
  where
    totalBits :: Int
    totalBits = components * lineWidth * fromEnum bitsPerComponent

    missingBits :: Int
    missingBits = (8 - (totalBits .&. 7)) .&. 7

{-|
Find all BitmapConfiguration that can be used for a given raw line width in
bytes.
-}
findBitmapConfigurations
  :: Int                   -- ^ Raw line width in bytes
  -> [BitmapConfiguration] -- ^ Possible bitmap configurations
findBitmapConfigurations rawLineWidth =
  [ BitmapConfiguration
      { bcLineWidth        = lineWidth
      , bcComponents       = components
      , bcBitsPerComponent = bitsPerComponent
      }
  | bitsPerComponent <- [BC2Bits, BC4Bits, BC8Bits, BC16Bits]
  , components       <- [1..4]
  , let totalBits = rawLineWidth * 8
        bitsPerPixel = components * fromEnum bitsPerComponent
  , bitsPerPixel > 0
  , totalBits `mod` bitsPerPixel == 0
  , let lineWidth = totalBits `div` bitsPerPixel
  ]
