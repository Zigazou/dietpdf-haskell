{-|
Determine the appropriate optimization pass for a `PDFObject`.

This module inspects object dictionaries and streams to classify optimizations
into categories:

* XML streams → `XMLOptimization`
* JPEG images (magic `\xff\xd8`) → `JPGOptimization`
* Graphics content streams → `GfxOptimization`
* TrueType font streams → `TTFOptimization`
* Object streams → `ObjectStreamOptimization`
* Cross-reference streams → `XRefStreamOptimization`
* Otherwise → `NoOptimization`

Detection uses keys like "Subtype" and "Type" and, when needed, parsers for
graphics (`gfxParse`) and TrueType fonts (`ttfParse`).
-}
module PDF.Processing.WhatOptimizationFor
  ( whatOptimizationFor
  )
where
import Data.ByteString qualified as BS
import Data.Logging (Logging)
import Data.PDF.OptimizationType
    ( OptimizationType (GfxOptimization, JPGOptimization, NoOptimization, ObjectStreamOptimization, TTFOptimization, XMLOptimization, XRefStreamOptimization)
    )
import Data.PDF.PDFObject (PDFObject (PDFName))
import Data.PDF.PDFWork (PDFWork, tryP)
import Data.Sequence qualified as SQ

import Font.TrueType.Parser.Font (ttfParse)

import PDF.Graphics.Parser.Stream (gfxParse)
import PDF.Object.State (getStream, getValue)


{-|
Classify a `PDFObject` into an `OptimizationType`.

Heuristics:

* `Subtype = XML` → XML optimization.
* `Subtype = Image` and stream begins with JPEG magic → JPG optimization.
* `Subtype = Form` → analyze stream with `gfxParse`; non-empty graphics →
  graphics optimization.
* `Type = ObjStm` → object stream optimization.
* `Type = XRef` → xref stream optimization.
* `Type = Pattern` → graphics optimization.
* If none of the above, attempt `ttfParse` on the stream; success → TTF
  optimization. Otherwise, analyze with `gfxParse` to decide graphics or no
  optimization.
-}
whatOptimizationFor :: Logging m => PDFObject -> PDFWork m OptimizationType
whatOptimizationFor object =
  getValue "Subtype" object >>= \case
    Just (PDFName "XML") -> return XMLOptimization
    Just (PDFName "Image") -> do
      stream <- getStream object
      if BS.take 2 stream == "\xff\xd8"
        then return JPGOptimization
        else return NoOptimization
    Just (PDFName "Form") -> do
        tryP (getStream object) >>= \case
          Right stream -> case gfxParse stream of
              Right SQ.Empty -> return NoOptimization
              Right _        -> return GfxOptimization
              _notGfx        -> return NoOptimization
          _noStream -> return NoOptimization
    _notXMLorImage -> getValue "Type" object >>= \case
      Just (PDFName "ObjStm")     -> return ObjectStreamOptimization
      Just (PDFName "XRef")       -> return XRefStreamOptimization
      Just (PDFName "Pattern")    -> return GfxOptimization
      Just (PDFName _unknownType) -> return NoOptimization
      _anyOtherCase -> do
          tryP (getStream object) >>= \case
            Right stream -> case ttfParse stream of
              Right _ttfFont -> return TTFOptimization
              _notTtfFont    -> case gfxParse stream of
                Right SQ.Empty -> return NoOptimization
                Right _        -> return GfxOptimization
                _notGfx        -> return NoOptimization
            _noStream -> return NoOptimization
