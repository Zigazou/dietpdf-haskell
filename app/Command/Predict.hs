module Command.Predict
  ( predictByteString
  ) where

import Codec.Compression.Predict (Entropy (EntropyShannon), Predictor, predict)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (throwE)

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Fallible (FallibleT)

predictByteString
  :: Predictor -> Int -> Int -> ByteString -> FallibleT IO ()
predictByteString predictor columns colors binData =
  case predict EntropyShannon predictor columns colors binData of
    (Right predicted) -> lift $ BS.putStr predicted
    (Left  err      ) -> throwE err
