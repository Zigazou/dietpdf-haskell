module Command.Encode
  ( encodeByteString
  ) where

import           Util.UnifiedError              ( FallibleT
                                                , UnifiedError
                                                  ( UnsupportedFeature
                                                  )
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
import qualified Codec.Filter.Ascii85          as A8
import qualified Codec.Filter.AsciiHex         as AH
import           Control.Monad.Trans.Except     ( throwE )
import           Control.Monad.Trans.Class      ( lift )


manage :: Either UnifiedError BS.ByteString -> FallibleT IO ()
manage (Right compressed) = lift $ BS.putStr compressed
manage (Left  err       ) = throwE err

encodeByteString :: Codec -> BS.ByteString -> FallibleT IO ()
encodeByteString RLE        binData = manage $ RL.compress binData
encodeByteString Deflate    binData = manage $ FL.fastCompress binData
encodeByteString NoCompress binData = manage $ FL.noCompress binData
encodeByteString Zopfli     binData = manage $ FL.compress binData
encodeByteString Ascii85    binData = manage $ A8.encode binData
encodeByteString Hex        binData = manage $ AH.encode binData
encodeByteString LZW _ = throwE (UnsupportedFeature "LZW compression")
