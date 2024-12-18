{-|
This modules implements the RLE alfgorithm as used in PDF file (and also TIFF)

From Adobe PDF 32000-1:2008:

@
    The RunLengthDecode filter decodes data that has been encoded in a simple
    byte-oriented format based on run length. The encoded data shall be a
    sequence of runs, where each run shall consist of a length byte followed by
    1 to 128 bytes of data. If the length byte is in the range 0 to 127, the
    following length + 1 (1 to 128) bytes shall be copied literally during
    decompression. If length is in the range 129 to 255, the following single
    byte shall be copied 257 - length (2 to 128) times during decompression.
    A length value of 128 shall denote EOD.

    The compression achieved by run-length encoding depends on the input data.
    In the best case (all zeros), a compression of approximately 64 : 1 is
    achieved for long files. The worst case (the hexadecimal sequence 00
    alternating with FF) results in an expansion of 127 : 128.
@
-}
module Codec.Compression.RunLength
  ( compress
  , entropyCompress
  , decompress
  ) where

import Codec.Compression.Flate (fastCompress)

import Control.Monad ((<=<))

import Data.Binary.Parser
    ( Get
    , anyWord8
    , endOfInput
    , getByteString
    , many'
    , parseOnly
    , peekMaybe
    , scan
    )
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Fallible (Fallible)
import Data.Kind (Type)
import Data.UnifiedError (UnifiedError (RLEDecodeError, RLEEncodeError))
import Data.Word (Word8)

import GHC.Base (maxInt)

type RLEAction :: Type
data RLEAction
  = RLERepeat !Int !Word8
  | RLECopy !ByteString
  | RLEEndOfData

rleEndOfData :: Word8
rleEndOfData = 128

decode :: RLEAction -> ByteString
decode RLEEndOfData            = ""
decode (RLECopy bytestring   ) = bytestring
decode (RLERepeat count value) = BS.replicate count value

encode :: RLEAction -> ByteString
encode RLEEndOfData = BS.singleton rleEndOfData
encode (RLECopy bytestring) =
  BS.concat [BS.singleton (fromIntegral (BS.length bytestring - 1)), bytestring]
encode (RLERepeat count value) = BS.pack [1 - fromIntegral count, value]

rleDecodeP :: Get RLEAction
rleDecodeP = anyWord8 >>= toRLEAction
 where
  toRLEAction :: Word8 -> Get RLEAction
  toRLEAction code = case compare code rleEndOfData of
    EQ -> return RLEEndOfData
    GT -> RLERepeat (fromIntegral (1 - code)) <$> anyWord8
    LT -> RLECopy <$> getByteString (fromIntegral (code + 1))

{-|
Decode a RLE bytestring.
-}
decompress :: ByteString -> Fallible ByteString
decompress stream =
  case
      BS.concat
      .   fmap decode
      <$> parseOnly (many' rleDecodeP <* endOfInput) stream
    of
      Left  msg     -> Left (RLEDecodeError msg)
      Right decoded -> Right decoded

upToNEqual :: (Int, Word8) -> Get ByteString
upToNEqual state = scan state updateState
 where
  updateState :: (Int, Word8) -> Word8 -> Maybe (Int, Word8)
  updateState (0, _) _ = Nothing
  updateState (count, previous) byte =
    if previous == byte then Just (count - 1, byte) else Nothing

upToNNotEqual :: (Int, Word8) -> Get ByteString
upToNNotEqual state = scan state updateState
 where
  updateState :: (Int, Word8) -> Word8 -> Maybe (Int, Word8)
  updateState (0, _) _ = Nothing
  updateState (count, previous) byte =
    if previous /= byte then Just (count - 1, byte) else Nothing


rleEncodeP :: Get RLEAction
rleEncodeP = do
  currentByte <- anyWord8
  nextByteM   <- peekMaybe
  case nextByteM of
    Just nextByte -> if nextByte == currentByte
      then do
        bytes <- upToNEqual (127, currentByte)
        return $! RLERepeat (1 + BS.length bytes) currentByte
      else do
        bytes <- upToNNotEqual (127, currentByte)
        return $! RLECopy (BS.cons currentByte bytes)
    Nothing       -> return $ RLECopy (BS.singleton currentByte)

{-|
Encode a bytestring into an RLE bytestring.
-}
compress :: ByteString -> Fallible ByteString
compress stream =
  case
      BS.concat
      .   fmap encode
      <$> parseOnly (many' rleEncodeP <* endOfInput) stream
    of
      Left  msg     -> Left (RLEEncodeError msg)
      Right encoded -> Right encoded

{-|
Gives a number showing the "entropy" of a `ByteString`.

The lower the number, the more compressible the `ByteString`.
-}
entropyCompress
  :: ByteString -- ^ A strict bytestring
  -> Double
entropyCompress = fromIntegral
                . either (const maxInt) BS.length
                . (fastCompress <=< compress)
