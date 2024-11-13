module PDF.Graphics.Optimize (optimizeGFX) where

import Control.Monad.State (gets)

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Search (indices)
import Data.Context (ctx)
import Data.Functor ((<&>))
import Data.Logging (Logging)
import Data.PDF.GFXObject (separateGfx)
import Data.PDF.PDFWork
  (PDFWork, getTranslationTable, sayComparisonP, sayP, withContext)
import Data.PDF.Program (extractObjects, parseProgram)
import Data.PDF.Settings
  (OptimizeGFX (DoNotOptimizeGFX, OptimizeGFX), Settings (sOptimizeGFX))
import Data.PDF.WorkData (wSettings)
import Data.Sequence qualified as SQ

-- TODO: import PDF.Graphics.Interpreter.OptimizeGState (optimizeGState)
import PDF.Graphics.Interpreter.OptimizeProgram (optimizeProgram)
import PDF.Graphics.Interpreter.RenameResources (renameResources)
import PDF.Graphics.Parser.Stream (gfxParse)

optimizeGFX :: Logging m => ByteString -> PDFWork m ByteString
optimizeGFX stream = do
  gets (sOptimizeGFX . wSettings) >>= \case
    DoNotOptimizeGFX -> return stream
    OptimizeGFX -> withContext (ctx ("optimizeGFX" :: String)) $
      if indices "/CIDInit" stream /= []
        then return stream
        else case gfxParse stream of
          Right SQ.Empty -> return stream

          Right objects -> do
            program <- getTranslationTable
                   <&> flip renameResources (parseProgram objects)

            optimizedProgram <- gets (`optimizeProgram` program)
                            -- TODO: >>= optimizeGState

            let optimizedStream = separateGfx
                                . extractObjects
                                $ optimizedProgram

            sayComparisonP
              "GFX stream optimization"
              (BS.length stream)
              (BS.length optimizedStream)

            return optimizedStream

          _error -> do
            sayP "GFX stream could not be parsed"
            return stream
