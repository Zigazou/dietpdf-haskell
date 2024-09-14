module Data.BitsArray
  ( BitsArray (BitsArray)
  , newBitsArray
  , appendBits
  , toByteString
  , fromByteString
  , word64ToWord8List
  ) where

import Control.Monad (forM_)

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString qualified as BS
import Data.Kind (Type)
import Data.Vector.Storable qualified as VS
import Data.Vector.Storable.ByteString (byteStringToVector, vectorToByteString)
import Data.Vector.Storable.Mutable qualified as MVS
import Data.Word (Word64, Word8)

import GHC.ST (ST)

type BitsArray :: Type
data BitsArray = BitsArray
  { baCursorOffset :: !Int
  , baCursorWidth  :: !Int
  , baLength       :: !Int
  , baBytes        :: !(VS.Vector Word8)
  }
  deriving stock (Eq, Show)

newBitsArray :: Int -> BitsArray
newBitsArray maxBytes = BitsArray
  { baCursorOffset = 0
  , baCursorWidth  = 1
  , baLength       = 0
  , baBytes        = VS.replicate maxBytes 0
  }

word64ToWord8List :: Int -> Word64 -> [Word8]
word64ToWord8List 0 _ = []
word64ToWord8List 1 x = fromIntegral <$> [x .&. 0xFF]
word64ToWord8List 2 x = fromIntegral <$> [(x `shiftR` 8) .&. 0xFF, x .&. 0xFF]
word64ToWord8List 3 x = fromIntegral <$> [(x `shiftR` 16) .&. 0xFF
                        , (x `shiftR` 8) .&. 0xFF
                        , x .&. 0xFF
                        ]
word64ToWord8List 4 x = fromIntegral <$> [(x `shiftR` 24) .&. 0xFF
                        , (x `shiftR` 16) .&. 0xFF
                        , (x `shiftR` 8) .&. 0xFF
                        , x .&. 0xFF
                        ]
word64ToWord8List 5 x = fromIntegral <$> [(x `shiftR` 32) .&. 0xFF
                        , (x `shiftR` 24) .&. 0xFF
                        , (x `shiftR` 16) .&. 0xFF
                        , (x `shiftR` 8) .&. 0xFF
                        , x .&. 0xFF
                        ]
word64ToWord8List 6 x = fromIntegral <$> [(x `shiftR` 40) .&. 0xFF
                        , (x `shiftR` 32) .&. 0xFF
                        , (x `shiftR` 24) .&. 0xFF
                        , (x `shiftR` 16) .&. 0xFF
                        , (x `shiftR` 8) .&. 0xFF
                        , x .&. 0xFF
                        ]
word64ToWord8List 7 x = fromIntegral <$> [(x `shiftR` 48) .&. 0xFF
                        , (x `shiftR` 40) .&. 0xFF
                        , (x `shiftR` 32) .&. 0xFF
                        , (x `shiftR` 24) .&. 0xFF
                        , (x `shiftR` 16) .&. 0xFF
                        , (x `shiftR` 8) .&. 0xFF
                        , x .&. 0xFF
                        ]
word64ToWord8List 8 x = fromIntegral <$> [(x `shiftR` 56) .&. 0xFF
                        , (x `shiftR` 48) .&. 0xFF
                        , (x `shiftR` 40) .&. 0xFF
                        , (x `shiftR` 32) .&. 0xFF
                        , (x `shiftR` 24) .&. 0xFF
                        , (x `shiftR` 16) .&. 0xFF
                        , (x `shiftR` 8) .&. 0xFF
                        , x .&. 0xFF
                        ]
word64ToWord8List _anyOtherLength x = word64ToWord8List 8 x

bitsSpan :: Int -> Int
bitsSpan len = (len `shiftR` 3)
             + if len .&. 7 == 0 then 0 else 1

appendBits :: Integral a => Int -> a -> BitsArray -> BitsArray
appendBits bitsWidth bits bitsArray =
  let leftShift  = (  8 - (bitsWidth .&. 7)
                    + 8 - (baLength bitsArray .&. 7)
                   ) .&. 7
      bits'      = fromIntegral bits `shiftL` leftShift :: Word64
      byteCount  = bitsSpan (bitsWidth + leftShift)
      bytes      = word64ToWord8List byteCount bits'
      byteOffset = baLength bitsArray `shiftR` 3

  in bitsArray { baLength = baLength bitsArray + bitsWidth
               , baBytes = VS.modify (appendBits' byteOffset bytes)
                                     (baBytes bitsArray)
               }
 where
  appendBits' :: Int -> [Word8] -> MVS.MVector s Word8 -> ST s ()
  appendBits' start bytes array = do
    forM_ (zip [start..] bytes) $ \(offset, byte) ->
      MVS.read array offset >>= MVS.write array offset . (byte .|.)

toByteString :: BitsArray -> BS.ByteString
toByteString bitsArray = BS.take (bitsSpan $ baLength bitsArray)
                                 (vectorToByteString (baBytes bitsArray))

fromByteString :: BS.ByteString -> BitsArray
fromByteString bytes = BitsArray
  { baCursorOffset = 0
  , baCursorWidth  = 1
  , baLength       = BS.length bytes * 8
  , baBytes        = byteStringToVector bytes
  }
