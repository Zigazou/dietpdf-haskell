{-|
Encodes byte strings using common PDF stream filters and writes the
encoded result to standard output within a 'FallibleT IO' context.

This module exposes 'encodeByteString', which dispatches to the appropriate
compressor or ASCII filter based on the selected 'Codec'. On success, the
encoded bytes are emitted to stdout; on failure, the error is propagated via
'throwE'.
-}
module Command.Encode
  ( encodeByteString
  ) where

import AppOptions
  (Codec (Ascii85, Brotli, Deflate, Hex, LZW, NoCompress, RLE, Zopfli))

import Codec.Compression.BrotliForPDF qualified as BR
import Codec.Compression.Flate qualified as FL
import Codec.Compression.LZW qualified as LZW
import Codec.Compression.RunLength qualified as RL
import Codec.Filter.Ascii85 qualified as A8
import Codec.Filter.AsciiHex qualified as AH

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (throwE)

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Fallible (Fallible, FallibleT)

{-|
Handles the result of an encoding operation.

* On 'Right', writes the encoded bytes to stdout using 'BS.putStr'.
* On 'Left', raises the encoding error in the 'FallibleT IO' monad via 'throwE'.

This helper centralizes IO emission and error propagation for the codec-specific
encoders.
-}
manage :: Fallible ByteString -> FallibleT IO ()
manage (Right compressed) = lift $ BS.putStr compressed
manage (Left  err       ) = throwE err

{-|
Encode a byte string according to the given 'Codec' and emit the encoded bytes
to standard output.

Input and output behavior:

* Input: a 'Codec' selector and the raw 'ByteString' data.
* Output: writes the encoded data to stdout or throws the encoding error.

Codec mappings used:

* 'RLE'        → run-length encoder ('RL.compress').
* 'Deflate'    → fast Deflate encoder ('FL.fastCompress').
* 'Brotli'     → Brotli encoder ('BR.compress').
* 'LZW'        → LZW encoder ('LZW.compress').
* 'NoCompress' → Deflate stream without compression ('FL.noCompress').
* 'Zopfli'     → high-density Deflate encoder ('FL.compress').
* 'Ascii85'    → ASCII85 encoder ('A8.encode').
* 'Hex'        → ASCII Hex encoder ('AH.encode').

Note: This function produces its result by writing to stdout rather than
returning the encoded bytes. Redirect stdout if you need to capture the output.
-}
encodeByteString :: Codec -> ByteString -> FallibleT IO ()
encodeByteString RLE        = manage . RL.compress
encodeByteString Deflate    = manage . FL.fastCompress
encodeByteString Brotli     = manage . BR.compress
encodeByteString LZW        = manage . LZW.compress
encodeByteString NoCompress = manage . FL.noCompress
encodeByteString Zopfli     = manage . FL.compress
encodeByteString Ascii85    = manage . A8.encode
encodeByteString Hex        = manage . AH.encode
