{-|
Preprocess JPEG images with the external dejpeg tool.
-}
module External.Dejpeg (dejpeg) where

import Data.ByteString (ByteString)
import Data.Fallible (FallibleT)

import External.ExternalCommand (externalCommandBuf'')

{-|
Run @dejpeg input.jpg output.png@ using real temporary files.
The temporary files are cleaned up after the command finishes.
-}
dejpeg :: ByteString -> FallibleT IO ByteString
dejpeg = externalCommandBuf'' "dejpeg" ["-", "-"] "jpg" "png"
