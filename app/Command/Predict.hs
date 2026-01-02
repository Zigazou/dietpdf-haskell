{-|
Applies a byte-wise prediction filter to input data and writes the result to
standard output.

This module exposes 'predictByteString', which invokes the predictor from
'Codec.Compression.Predict' using Shannon entropy ('EntropyShannon') to guide
transform selection. It is useful for experimenting with PNG-style predictors
and related bytewise transforms given image layout parameters.
-}
module Command.Predict
  ( predictByteString
  ) where

import Codec.Compression.Predict (Entropy (EntropyShannon), Predictor, predict)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (throwE)

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Fallible (FallibleT)

{-|
Run a prediction transform on the given data and emit the transformed bytes to
standard output.

Parameters:

* 'Predictor': the prediction algorithm to apply (e.g., None, Sub, Up, Avg,
  Paeth).
* 'Int' (columns): number of columns in the input raster.
* 'Int' (colors): number of color components per pixel (e.g., 1 for grayscale,
  3 for RGB).
* 'ByteString': input data to transform.

Behavior:

* Calls 'predict' with 'EntropyShannon' to compute the predicted output.
* On success, writes the predicted bytes to stdout; on failure, propagates the
  error via 'throwE'.

Side effects: writes to stdout within the 'FallibleT IO' context.
-}
predictByteString
  :: Predictor -> Int -> Int -> ByteString -> FallibleT IO ()
predictByteString predictor columns colors binData =
  case predict EntropyShannon predictor columns colors binData of
    (Right predicted) -> lift $ BS.putStr predicted
    (Left  err      ) -> throwE err
