module Decode
  ( decodeByteString
  ) where

import           Util.UnifiedError              ( FallibleT
                                                , UnifiedError
                                                )
import qualified Data.ByteString               as BS
import           AppOptions                     ( Codec
                                                  ( LZW
                                                  , RLE
                                                  , Deflate
                                                  , Zopfli
                                                  , NoCompress
                                                  , Ascii85
                                                  , Hex
                                                  )
                                                )
import qualified Codec.Compression.Flate       as FL
import qualified Codec.Compression.RunLength   as RL
import qualified Codec.Compression.LZW         as LZ
import qualified Codec.Filter.Ascii85          as A8
import qualified Codec.Filter.AsciiHex         as AH
import           Control.Monad.Trans.Except     ( throwE )
import           Control.Monad.Trans.Class      ( lift )


manage :: Either UnifiedError BS.ByteString -> FallibleT IO ()
manage (Right compressed) = lift $ BS.putStr compressed
manage (Left  err       ) = throwE err

decodeByteString :: Codec -> BS.ByteString -> FallibleT IO ()
decodeByteString RLE        binData = manage $ RL.decompress binData
decodeByteString Deflate    binData = manage $ FL.decompress binData
decodeByteString NoCompress binData = manage $ FL.decompress binData
decodeByteString Zopfli     binData = manage $ FL.decompress binData
decodeByteString Ascii85    binData = manage $ A8.decode binData
decodeByteString Hex        binData = manage $ AH.decode binData
decodeByteString LZW        binData = manage $ LZ.decompress binData
