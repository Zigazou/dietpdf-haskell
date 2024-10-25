module Command.Decode
  ( decodeByteString
  ) where

import AppOptions (Codec (Ascii85, Deflate, Hex, LZW, NoCompress, RLE, Zopfli))

import Codec.Compression.Flate qualified as FL
import Codec.Compression.LZW qualified as LZ
import Codec.Compression.RunLength qualified as RL
import Codec.Filter.Ascii85 qualified as A8
import Codec.Filter.AsciiHex qualified as AH

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (throwE)

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Fallible (Fallible, FallibleT)

manage :: Fallible ByteString -> FallibleT IO ()
manage (Right compressed) = lift $ BS.putStr compressed
manage (Left  err       ) = throwE err

decodeByteString :: Codec -> ByteString -> FallibleT IO ()
decodeByteString RLE        binData = manage $ RL.decompress binData
decodeByteString Deflate    binData = manage $ FL.decompress binData
decodeByteString NoCompress binData = manage $ FL.decompress binData
decodeByteString Zopfli     binData = manage $ FL.decompress binData
decodeByteString Ascii85    binData = manage $ A8.decode binData
decodeByteString Hex        binData = manage $ AH.decode binData
decodeByteString LZW        binData = manage $ LZ.decompress binData
