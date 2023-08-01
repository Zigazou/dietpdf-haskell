module Command.Predict
  ( predictByteString
  ) where

import           Util.UnifiedError              ( FallibleT )
import qualified Data.ByteString               as BS
import           Codec.Compression.Predictor    ( predict
                                                , Predictor
                                                , EntropyType(EntropyShannon)
                                                )
import           Control.Monad.Trans.Except     ( throwE )
import           Control.Monad.Trans.Class      ( lift )

predictByteString
  :: Predictor -> Int -> Int -> BS.ByteString -> FallibleT IO ()
predictByteString predictor columns colors binData =
  case predict EntropyShannon predictor columns colors binData of
    (Right predicted) -> lift $ BS.putStr predicted
    (Left  err      ) -> throwE err
