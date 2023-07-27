module Unpredict
  ( unpredictByteString
  ) where

import           Util.UnifiedError              ( FallibleT )
import qualified Data.ByteString               as BS
import           Codec.Compression.Predictor    ( unpredict
                                                , Predictor
                                                )
import           Control.Monad.Trans.Except     ( throwE )
import           Control.Monad.Trans.Class      ( lift )

unpredictByteString
  :: Predictor -> Int -> Int -> BS.ByteString -> FallibleT IO ()
unpredictByteString predictor columns colors binData =
  case unpredict predictor columns colors binData of
    (Right predicted) -> lift $ BS.putStr predicted
    (Left  err      ) -> throwE err
