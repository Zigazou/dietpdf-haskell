{-|
Reverses a prediction filter on input data and writes the result to standard
output.

This module exposes 'unpredictByteString', which applies the inverse transform
for a given 'Predictor' from 'Codec.Compression.Predict'. It is complementary
to prediction-based encoders (e.g., PNG-style predictors), restoring original
bytes from their predicted form given layout parameters.
-}
module Command.Unpredict
  ( unpredictByteString
  ) where

import Codec.Compression.Predict (Predictor, unpredict)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (throwE)

import Data.Bitmap.BitmapConfiguration
  ( BitmapConfiguration (BitmapConfiguration, bcBitsPerComponent, bcComponents, bcLineWidth)
  )
import Data.Bitmap.BitsPerComponent (BitsPerComponent (BC8Bits))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Fallible (FallibleT)

{-|
Undo a prediction transform and emit the unpredicted bytes to standard output.

Parameters:

* 'Predictor': the prediction algorithm used originally (e.g., Sub, Up, Avg,
  Paeth).
* 'Int' (columns): number of columns in the input raster.
* 'Int' (colors): number of color components per pixel.
* 'ByteString': input data to invert.

Behavior:

* Calls 'unpredict' with the provided parameters to restore original bytes.
* On success, writes the unpredicted bytes to stdout; on failure, propagates
  the error via 'throwE'.

Side effects: writes to stdout within the 'FallibleT IO' context.
-}
unpredictByteString
  :: Predictor -> Int -> Int -> ByteString -> FallibleT IO ()
unpredictByteString predictor columns colors binData =
  let bitmapConfig = BitmapConfiguration
        { bcLineWidth        = columns
        , bcComponents       = colors
        , bcBitsPerComponent = BC8Bits
        }
  in case unpredict predictor bitmapConfig binData of
    (Right predicted) -> lift $ BS.putStr predicted
    (Left  err      ) -> throwE err
