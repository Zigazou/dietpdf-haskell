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


{- |
Determine the optimization type that can be applied to a `PDFObject`.
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
