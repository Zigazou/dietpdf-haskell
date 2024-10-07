module Pdf.Graphics.Optimize (optimizeGFX) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Search (indices)
import Data.Context (Context)
import Data.Fallible (FallibleT)
import Data.Logging (Logging, sayComparisonF, sayF)
import Data.Sequence qualified as SQ
import Data.TranslationTable (TranslationTable)

import Pdf.Graphics.Interpreter.Program
    ( extractObjects
    , optimizeProgram
    , parseProgram
    )
import Pdf.Graphics.Object (separateGfx)
import Pdf.Graphics.Parser.Stream (gfxParse)
import Pdf.Graphics.RenameResources (renameResourcesInObject)

optimizeGFX
  :: Logging m
  => Context
  -> TranslationTable ByteString
  -> ByteString
  -> FallibleT m ByteString
optimizeGFX context nameTranslations stream = if indices "/CIDInit" stream /= []
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

        sayComparisonF
          context
          "GFX stream optimization"
          (BS.length stream)
          (BS.length optimizedStream)

        return optimizedStream

      _error -> do
          sayF context "GFX stream could not be parsed"
          return stream
