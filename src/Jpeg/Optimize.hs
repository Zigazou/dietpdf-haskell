module Jpeg.Optimize (optimizeJPG) where

import Data.ByteString qualified as BS
import Data.Sequence qualified as SQ

import Jpeg.Jpeg (encode, jpgParse)

import Util.Logging (Logging, sayComparisonF, sayF)
import Util.UnifiedError (FallibleT)

optimizeJPG :: Logging m => BS.ByteString -> FallibleT m BS.ByteString
optimizeJPG stream =
    case jpgParse stream of
      Right SQ.Empty -> do
        sayF "  - No JPG optimization for this object"
        return stream

      Right jpeg -> do
        let optimizedStream = encode jpeg

        sayComparisonF "JPG stream optimization"
                        (BS.length stream)
                        (BS.length optimizedStream)

        return optimizedStream

      _error -> do
          sayF "  - JPG stream could not be parsed"
          return stream
