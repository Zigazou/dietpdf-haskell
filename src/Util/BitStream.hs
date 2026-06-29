module Util.BitStream
  ( BitStream (BitStream, bsBitIndex, bsBytes)
  , BitStreamS
  , restart
  , restartS
  , fromByteString
  , readBit
  , readBitS
  ) where

import Control.Monad.State (State, get, put, state)

import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Kind (Type)

type BitStream :: Type
data BitStream = BitStream
  { bsBitIndex  :: !Int        -- ^ Current bit index in the current byte.
  , bsByteIndex :: !Int        -- ^ Current byte index.
  , bsBytes     :: !ByteString -- ^ Original ByteString.
  } deriving stock (Show, Eq)

type BitStreamS :: Type -> Type
type BitStreamS a = State BitStream a

byteMasks :: ByteString
byteMasks = "\x80\x40\x20\x10\x08\x04\x02\x01"

restart :: BitStream -> BitStream
restart (BitStream _bitIndex _byteIndex bytes) = BitStream 0 0 bytes

restartS :: BitStreamS ()
restartS = get >>= put . restart

fromByteString :: ByteString -> BitStream
fromByteString = BitStream 0 0

readBit :: BitStream -> (Maybe Bool, BitStream)
readBit (BitStream bitIndex byteIndex bytes)
  | byteIndex >= BS.length bytes =
      ( Nothing
      , BitStream bitIndex byteIndex bytes
      )
  | bitIndex == 7 =
      ( Just $ BS.index bytes byteIndex .&. mask /= 0
      , BitStream 0 (byteIndex + 1) bytes
      )
  | otherwise =
      ( Just $ BS.index bytes byteIndex .&. mask /= 0
      , BitStream (bitIndex + 1) byteIndex bytes
      )
  where 
    mask = BS.index byteMasks bitIndex

readBitS :: BitStreamS (Maybe Bool)
readBitS = state readBit
