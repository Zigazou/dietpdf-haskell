{-|
Optimize resource names in PDF documents.

Provides utilities for renaming resource objects (fonts, images, graphics
states) to shorter identifiers to reduce file size. Resource renaming is
performed based on a translation table when graphics optimization is enabled.
-}
module PDF.Document.OptimizeResources
  ( optimizeResources
  ) where

import Control.Monad.State (gets)

import Data.Logging (Logging)
import Data.Map.Strict qualified as Map
import Data.PDF.PDFObjects (toPDFDocument)
import Data.PDF.PDFPartition
  (PDFPartition (ppObjectsWithStream, ppObjectsWithoutStream))
import Data.PDF.PDFWork
  (PDFWork, modifyIndirectObjects, sayP, setTranslationTable)
import Data.PDF.Resource (toNameBase)
import Data.PDF.Settings
  (OptimizeGFX (DoNotOptimizeGFX, OptimizeGFX), Settings (sOptimizeGFX))
import Data.PDF.WorkData (WorkData (wPDF, wSettings))
import Data.TranslationTable (getTranslationTable)

import PDF.Document.Resources (getAllResourceNames)
import PDF.Object.Object.RenameResources (containsResources, renameResources)

{-|
Optimize resource names throughout a PDF document.

Extracts all resource names from the document and generates a translation table
if graphics optimization is enabled. Resources are renamed to shorter
identifiers and all references are updated. Non-graphics-optimized documents
skip translation to preserve original resource names. Logs progress of
optimization stages.
-}
optimizeResources :: Logging m => PDFWork m ()
optimizeResources = do
  sayP "Optimizing resource names"

  resourceNames <- getAllResourceNames

  -- Do not create a translation table if GFX won't be optimized.
  optGFX <- gets (sOptimizeGFX . wSettings)
  let nameTranslations = case optGFX of
        OptimizeGFX      -> getTranslationTable toNameBase resourceNames
        DoNotOptimizeGFX -> Map.empty

  setTranslationTable nameTranslations

  sayP "Renaming resources"

  -- Get all objects containing resources.
  wosContainingResources <- gets ( toPDFDocument
                                 . ppObjectsWithoutStream
                                 . wPDF
                                 ) >>= containsResources

  wsContainingResources <- gets ( toPDFDocument
                                . ppObjectsWithStream
                                . wPDF
                                ) >>= containsResources

  let containingResources = wosContainingResources <> wsContainingResources

  -- Rename resources in all objects.
  modifyIndirectObjects (renameResources nameTranslations containingResources)
