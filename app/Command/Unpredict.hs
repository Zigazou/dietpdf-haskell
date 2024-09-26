module Command.Unpredict
  ( unpredictByteString
  ) where

import Codec.Compression.Predict (Predictor, unpredict)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (throwE)

import Data.ByteString qualified as BS
import Data.Fallible (FallibleT)

unpredictByteString
  :: Predictor -> Int -> Int -> BS.ByteString -> FallibleT IO ()
unpredictByteString predictor columns colors binData =
  case unpredict predictor columns colors binData of
    (Right predicted) -> lift $ BS.putStr predicted
    (Left  err      ) -> throwE err
