module Command.Encode
  ( encodeByteString
  ) where

import AppOptions (Codec (Ascii85, Deflate, Hex, LZW, NoCompress, RLE, Zopfli))

import Codec.Compression.Flate qualified as FL
import Codec.Compression.LZW qualified as LZW
import Codec.Compression.RunLength qualified as RL
import Codec.Filter.Ascii85 qualified as A8
import Codec.Filter.AsciiHex qualified as AH

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (throwE)

import Data.ByteString qualified as BS
import Data.UnifiedError (FallibleT, UnifiedError)


manage :: Either UnifiedError BS.ByteString -> FallibleT IO ()
manage (Right compressed) = lift $ BS.putStr compressed
manage (Left  err       ) = throwE err

encodeByteString :: Codec -> BS.ByteString -> FallibleT IO ()
encodeByteString RLE        binData = manage $ RL.compress binData
encodeByteString Deflate    binData = manage $ FL.fastCompress binData
encodeByteString LZW        binData = manage $ LZW.compress binData
encodeByteString NoCompress binData = manage $ FL.noCompress binData
encodeByteString Zopfli     binData = manage $ FL.compress binData
encodeByteString Ascii85    binData = manage $ A8.encode binData
encodeByteString Hex        binData = manage $ AH.encode binData
