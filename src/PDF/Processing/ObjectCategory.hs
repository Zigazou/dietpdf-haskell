{-|
Classify `PDFObject`s into high-level categories for reporting.

This module provides `objectCategory`, which determines whether an object is
`XML`, `Vector` graphics, `Bitmap` image, `Font`, `File`, or `Other`. It
consults dictionary keys ("Type", "Subtype") and, when possible, attempts to
unfilter and analyze streams to improve classification.
-}
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

{-|
Determine the `ObjectCategory` for a `PDFObject`.

Behavior:

* Attempts to read the object's dictionary; if unavailable, returns `Other`.
* Tries to unfilter the object and classify via `whatOptimizationFor` to handle
  malformed streams that might otherwise prevent dictionary inspection; on
  failure, falls back to dictionary-based heuristics.
* Checks `Type`/`Subtype` for embedded files, images, forms, fonts, and XML.
-}
objectCategory :: Logging IO => PDFObject -> PDFWork IO ObjectCategory
objectCategory object = tryP (getDictionary object) >>= \case
  Left _noDictionary -> return Other
  Right dict -> do
    tryP (unfilter object >>= whatOptimizationFor) >>= \case
      Left _unfilterOrOptError -> return Other
      Right optimization -> case optimization of
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
