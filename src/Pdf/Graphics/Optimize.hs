module Pdf.Graphics.Optimize (optimizeGFX) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Search (indices)
import Data.Context (ctx)
import Data.Logging (Logging)
import Data.PDF.GFXObject (separateGfx)
import Data.PDF.PDFWork (PDFWork, sayComparisonP, sayP, withContext)
import Data.Sequence qualified as SQ
import Data.TranslationTable (TranslationTable)

import Pdf.Graphics.Interpreter.OptimizeProgram (optimizeProgram)
import Pdf.Graphics.Interpreter.Program (extractObjects, parseProgram)
import Pdf.Graphics.Parser.Stream (gfxParse)
import Pdf.Graphics.RenameResources (renameResourcesInObject)

optimizeGFX
  :: Logging m
  => TranslationTable ByteString
  -> ByteString
  -> PDFWork m ByteString
optimizeGFX nameTranslations stream =
  withContext (ctx ("optimizeGFX" :: String)) $ do
    if indices "/CIDInit" stream /= []
    then return stream
    else
      case gfxParse stream of
        Right SQ.Empty -> return stream

        Right objects -> do
          let optimizedStream = separateGfx
                              . fmap (renameResourcesInObject nameTranslations)
                              . extractObjects
                              . optimizeProgram
                              . parseProgram
                              $ objects

          sayComparisonP
            "GFX stream optimization"
            (BS.length stream)
            (BS.length optimizedStream)

          return optimizedStream

        _error -> do
            sayP "GFX stream could not be parsed"
            return stream
