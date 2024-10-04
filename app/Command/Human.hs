module Command.Human
  ( humanByteString
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (throwE)

import Data.ByteString qualified as BS
import Data.Fallible (FallibleT)
import Data.Text.IO qualified as TIO

import Pdf.Graphics.Interpreter.Human (human)
import Pdf.Graphics.Interpreter.Program (parseProgram)
import Pdf.Graphics.Parser.Stream (gfxParse)

humanByteString :: BS.ByteString -> FallibleT IO ()
humanByteString code = case gfxParse code of
  (Left  err       ) -> throwE err
  (Right gfxObjects) -> do
    let program = parseProgram gfxObjects
    lift $ TIO.putStr (human 0 program)
