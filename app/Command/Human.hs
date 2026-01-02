{-|
Renders a human-readable view of a PDF graphics content stream.

This module exposes 'humanByteString', which parses a raw graphics stream into
graphics objects ('gfxParse'), builds a 'Program' ('parseProgram'), and formats
it using 'human' before writing the result to standard output. Parse errors are
propagated through the 'FallibleT IO' monad using 'throwE'.
-}
module Command.Human
  ( humanByteString
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (throwE)

import Data.ByteString (ByteString)
import Data.Fallible (FallibleT)
import Data.PDF.Program (parseProgram)
import Data.Text.IO qualified as TIO

import PDF.Graphics.Interpreter.Human (human)
import PDF.Graphics.Parser.Stream (gfxParse)

{-|
Convert a raw graphics content stream to human-readable text and emit it to
standard output.

Behavior:

* Parses the input 'ByteString' with 'gfxParse'. On failure, raises the parse
  error via 'throwE'.
* On success, builds a 'Program' with 'parseProgram' and formats it with
  'human', starting at indentation level @0@, then writes the resulting text to
  stdout.

Side effects: writes to stdout within the 'FallibleT IO' context.
-}
humanByteString :: ByteString -> FallibleT IO ()
humanByteString code = case gfxParse code of
  (Left  err       ) -> throwE err
  (Right gfxObjects) -> do
    let program = parseProgram gfxObjects
    lift $ TIO.putStr (human 0 program)
