module PDF.Processing.ObjectCategory (objectCategory) where

import Data.Logging (Logging)
import Data.Map qualified as Map
import Data.PDF.ObjectCategory
  (ObjectCategory (Bitmap, File, Font, Other, Vector, XML))
import Data.PDF.OptimizationType
  ( OptimizationType (GfxOptimization, JPGOptimization, TTFOptimization, XMLOptimization)
  )
import Data.PDF.PDFObject (PDFObject (PDFName))
import Data.PDF.PDFWork (PDFWork, tryP)

import PDF.Object.State (getDictionary)
import PDF.Processing.Unfilter (unfilter)
import PDF.Processing.WhatOptimizationFor (whatOptimizationFor)

objectCategory :: Logging IO => PDFObject -> PDFWork IO ObjectCategory
objectCategory object = tryP (getDictionary object) >>= \case
  Left _noDictionary -> return Other
  Right dict -> do
    optimization <- unfilter object >>= whatOptimizationFor
    case optimization of
      XMLOptimization -> return XML
      GfxOptimization -> return Vector
      JPGOptimization -> return Bitmap
      TTFOptimization -> return Font
      _noOptimization -> case Map.lookup "Type" dict of
        (Just (PDFName "EmbeddedFile")) -> return File
        (Just (PDFName "XObject")) ->
          case Map.lookup "Subtype" dict of
            (Just (PDFName "Image"))        -> return Bitmap
            (Just (PDFName "Form"))         -> return Vector
            (Just (PDFName "Type1"))        -> return Font
            (Just (PDFName "EmbeddedFile")) -> return File
            (Just (PDFName "XML"))          -> return XML
            _anyOtherSubType                -> return Other
        (Just (PDFName "Font")) -> return Font
        (Just (PDFName "XML"))  -> return XML
        _anyOtherType           -> case Map.lookup "Subtype" dict of
            (Just (PDFName "Image"))        -> return Bitmap
            (Just (PDFName "Form"))         -> return Vector
            (Just (PDFName "Type1"))        -> return Font
            (Just (PDFName "EmbeddedFile")) -> return File
            (Just (PDFName "XML"))          -> return XML
            _anyOtherSubType                -> return Other

