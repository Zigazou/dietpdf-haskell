{-|
Optimization of PDF graphics streams

This module provides optimization functionality for PDF graphics streams. It
coordinates the parsing, analysis, and transformation of graphics stream objects
to reduce file size while maintaining visual equivalence.
-}
module PDF.Graphics.Optimize (optimizeGFX, optimizeGFXWithTextState) where

import Control.Monad.State (get, gets, put)
import Control.Parallel.Strategies (evalTuple2, parMap, r0, rdeepseq)

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Search (indices)
import Data.Context (ctx)
import Data.Functor ((<&>))
import Data.List (minimumBy)
import Data.Logging (Logging)
import Data.Ord (comparing)
import Data.PDF.GFXObject (separateGfx)
import Data.PDF.PDFObject (PDFObject)
import Data.PDF.PDFPartition (ppObjectsWithStream, ppObjectsWithoutStream, PDFPartition)
import Data.PDF.PDFWork
  (PDFWork, getTranslationTable, sayComparisonP, sayP, withContext)
import Data.PDF.Program (extractObjects, parseProgram, Program)
import Data.PDF.Settings
  (OptimizeGFX (DoNotOptimizeGFX, OptimizeGFX), Settings (sOptimizeGFX))
import Data.PDF.WorkData (WorkData, wPDF, wSettings)
import Data.Sequence qualified as SQ

import PDF.Graphics.Interpreter.OptimizeGState (gStateCost, planGState)
import PDF.Graphics.Interpreter.OptimizeProgram
  (optimizeProgramWithTextResources)
import PDF.Graphics.Interpreter.OptimizeScale (optimizeScale)
import PDF.Graphics.Interpreter.RenameResources (renameResources)
import PDF.Graphics.Parser.Stream (gfxParse)
import PDF.Graphics.TextMetrics (buildTextResources, TextResources)

import Util.Dictionary (Dictionary)

{-|
Optimize a PDF graphics stream to reduce file size.

This function performs the following optimization steps:

1. Checks the optimization setting; returns the stream unchanged if optimization
   is disabled
2. Returns the stream unchanged if it contains CIDInit resources (which should
   not be optimized)
3. Parses the graphics stream into individual objects
4. Extracts the translation table for resource renaming
5. Renames resources to use shorter identifiers
6. Optimizes and factorizes scale candidates in parallel
7. Selects the lowest serialized stream and resource cost, committing only its
   resources
8. Converts the optimized objects back to a byte stream
9. Logs the file size reduction achieved
10. Returns the optimized stream, or the original stream if parsing fails

__Parameters:__

- Resolved resources, or Nothing for unknown or ambiguous ownership. The generic
  entry point conservatively assumes inherited text parameters.
- The raw PDF graphics stream as a bytestring

__Returns:__

- The optimized graphics stream (possibly smaller), or the original stream if
  optimization could not be applied
-}
optimizeGFX
  :: Logging m
  => Maybe (Dictionary PDFObject)
  -> ByteString
  -> PDFWork m ByteString
optimizeGFX = optimizeGFXWithTextState True

-- | Select inherited Form parameters or the PDF page defaults explicitly.
optimizeGFXWithTextState
  :: Logging m
  => Bool
  -> Maybe (Dictionary PDFObject)
  -> ByteString
  -> PDFWork m ByteString
optimizeGFXWithTextState inherited resources stream = do
  gets (sOptimizeGFX . wSettings) >>= \case
    DoNotOptimizeGFX -> return stream
    OptimizeGFX -> withContext (ctx ("optimizeGFX" :: String)) $
      if indices "/CIDInit" stream /= []
        then
          return stream
        else
          case gfxParse stream of
            Right SQ.Empty -> return stream

            Right objects -> do
              program <- getTranslationTable
                    <&> flip renameResources (parseProgram objects)

              -- Try multiple scale optimizations in parallel.
              workData <- get
              let
                pdf :: PDFPartition
                pdf = wPDF workData

                textResources :: TextResources
                textResources =
                  buildTextResources
                    (ppObjectsWithoutStream pdf <> ppObjectsWithStream pdf)
                    resources

                optimizeAtScale :: Double -> (Int, (Program, WorkData))
                optimizeAtScale scale =
                  let
                    optimized :: Program
                    optimized = optimizeProgramWithTextResources
                                  textResources
                                  inherited
                                  workData
                                  (optimizeScale scale program)

                    factorized :: Program
                    candidateWork :: WorkData
                    (factorized, candidateWork) = planGState workData optimized

                    size :: Int
                    size = gStateCost candidateWork factorized
                  in
                    (size, (factorized, candidateWork))

                optimizedPrograms =
                  parMap (evalTuple2 rdeepseq r0) optimizeAtScale
                    -- Inherited font size and spacing cannot be rescaled by
                    -- changing only the operands inside a Form's stream.
                    (if inherited then [1.0] else [1.0, 10.0, 100.0, 1000.0])

              -- Register graphics state resources only for the selected
              -- program.
              let
                optimizedProgram :: Program
                selectedWork :: WorkData
                (optimizedProgram, selectedWork) =
                    snd (minimumBy (comparing fst) optimizedPrograms)

              put selectedWork

              let
                optimizedStream :: ByteString
                optimizedStream = separateGfx (extractObjects optimizedProgram)

              sayComparisonP
                "GFX stream optimization"
                (BS.length stream)
                (BS.length optimizedStream)

              return optimizedStream

            _error -> do
              sayP "GFX stream could not be parsed"
              return stream
