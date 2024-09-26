module Pdf.Object.OptimizationType
  ( OptimizationType(XMLOptimization, GfxOptimization, ObjectStreamOptimization, XRefStreamOptimization, JPGOptimization, NoOptimization, TTFOptimization)
  , whatOptimizationFor
  )
where
import Data.ByteString qualified as BS
import Data.Fallible (FallibleT, tryF)
import Data.Kind (Type)
import Data.Logging (Logging)
import Data.Sequence qualified as SQ

import Font.TrueType.Parser.Font (ttfParse)

import Pdf.Graphics.Parser.Stream (gfxParse)
import Pdf.Object.Object (PDFObject (PDFName))
import Pdf.Object.State (getStream, getValue)

type OptimizationType :: Type
data OptimizationType = XMLOptimization
                      | GfxOptimization
                      | ObjectStreamOptimization
                      | XRefStreamOptimization
                      | JPGOptimization
                      | TTFOptimization
                      | NoOptimization
                      deriving stock (Eq)

{- |
Determine the optimization type that can be applied to a `PDFObject`.
-}
whatOptimizationFor :: Logging m => PDFObject -> FallibleT m OptimizationType
whatOptimizationFor object =
  getValue "Subtype" object >>= \case
    Just (PDFName "XML") -> return XMLOptimization
    Just (PDFName "Image") -> do
      stream <- getStream object
      if BS.take 2 stream == "\xff\xd8"
        then return JPGOptimization
        else return NoOptimization
    _notXMLorImage -> getValue "Type" object >>= \case
      Just (PDFName "ObjStm")     -> return ObjectStreamOptimization
      Just (PDFName "XRef")       -> return XRefStreamOptimization
      Just (PDFName _unknownType) -> return NoOptimization
      _anyOtherCase -> do
          tryF (getStream object) >>= \case
            Right stream -> case ttfParse stream of
              Right _ttfFont -> return TTFOptimization
              _notTtfFont    -> case gfxParse stream of
                Right SQ.Empty -> return NoOptimization
                Right _        -> return GfxOptimization
                _notGfx        -> return NoOptimization
            _noStream -> return NoOptimization
