-- | Decoded image samples together with their original encoding.
module Data.Bitmap.Bitmap
  ( Bitmap (Bitmap, bitmapConfiguration, bitmapHeight, bitmapSamples, bitmapOriginalEncoding)
  , BitmapEncoding (RawEncoding, JPEGEncoding, JPEG2000Encoding)
  ) where

import Data.Bitmap.BitmapConfiguration (BitmapConfiguration)
import Data.ByteString (ByteString)
import Data.Kind (Type)

type BitmapEncoding :: Type
data BitmapEncoding
  = RawEncoding
  | JPEGEncoding Int -- ^ JPEG quality inferred from its quantization tables.
  | JPEG2000Encoding
  deriving stock (Eq, Show)

type Bitmap :: Type
data Bitmap = Bitmap
  { bitmapConfiguration    :: !BitmapConfiguration
  , bitmapHeight           :: !Int
  , bitmapSamples          :: !ByteString
  , bitmapOriginalEncoding :: !BitmapEncoding
  }
  deriving stock (Eq, Show)
