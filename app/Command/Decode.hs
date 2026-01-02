{-|
Decodes byte strings using common PDF stream filters and writes the
decoded result to standard output within a 'FallibleT IO' context.

This module exposes 'decodeByteString', which dispatches to the appropriate
decompressor or ASCII filter based on the selected 'Codec'. On success, the
decoded bytes are emitted to stdout; on failure, the error is propagated via
'throwE'.
-}
module Command.Decode
  ( decodeByteString
  ) where

import AppOptions (Codec (Ascii85, Deflate, Hex, LZW, NoCompress, RLE, Zopfli))

import Codec.Compression.Flate qualified as FL
import Codec.Compression.LZW qualified as LZ
import Codec.Compression.RunLength qualified as RL
import Codec.Filter.Ascii85 qualified as A8
import Codec.Filter.AsciiHex qualified as AH

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (throwE)

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Fallible (Fallible, FallibleT)

{-|
Handles the result of a decoding operation.

* On 'Right', writes the decoded bytes to stdout using 'BS.putStr'.
* On 'Left', raises the decoding error in the 'FallibleT IO' monad via 'throwE'.

This helper centralizes IO emission and error propagation for the codec-specific
decoders.
-}
manage :: Fallible ByteString -> FallibleT IO ()
manage (Right compressed) = lift $ BS.putStr compressed
manage (Left  err       ) = throwE err

{-|
Decode a byte string according to the given 'Codec' and emit the decoded bytes
to standard output.

Input and output behavior:

* Input: a 'Codec' selector and the raw 'ByteString' data.
* Output: writes the decoded data to stdout or throws the decoding error.

Codec mappings used:

* 'RLE'        → run-length decoder ('RL.decompress').
* 'Deflate'    → Deflate/Flate decoder ('FL.decompress').
* 'NoCompress' → Deflate decoder path (currently routed to 'FL.decompress').
* 'Zopfli'     → Deflate decoder path (Zopfli-compressed data is Deflate-compatible).
* 'Ascii85'    → ASCII85 decoder ('A8.decode').
* 'Hex'        → ASCII Hex decoder ('AH.decode').
* 'LZW'        → LZW decoder ('LZ.decompress').

Note: This function produces its result by writing to stdout rather than
returning the decoded bytes. Redirect stdout if you need to capture the output.
-}
decodeByteString :: Codec -> ByteString -> FallibleT IO ()
decodeByteString RLE        binData = manage $ RL.decompress binData
decodeByteString Deflate    binData = manage $ FL.decompress binData
decodeByteString NoCompress binData = manage $ FL.decompress binData
decodeByteString Zopfli     binData = manage $ FL.decompress binData
decodeByteString Ascii85    binData = manage $ A8.decode binData
decodeByteString Hex        binData = manage $ AH.decode binData
decodeByteString LZW        binData = manage $ LZ.decompress binData
