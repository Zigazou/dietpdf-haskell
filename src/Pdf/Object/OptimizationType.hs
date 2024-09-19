module Pdf.Object.OptimizationType
  ( OptimizationType(XMLOptimization, GfxOptimization, ObjectStreamOptimization, XRefStreamOptimization, JPGOptimization, NoOptimization)
  , whatOptimizationFor
  )
where
import Data.ByteString qualified as BS
import Data.Kind (Type)
import Data.Sequence qualified as SQ

import Pdf.Graphics.Parser.Stream (gfxParse)
import Pdf.Object.Object (PDFObject (PDFName), hasKey)
import Pdf.Object.State (getStream, getValue)

import Util.Logging (Logging)
import Util.UnifiedError (FallibleT, tryF)

type OptimizationType :: Type
data OptimizationType = XMLOptimization
                      | GfxOptimization
                      | ObjectStreamOptimization
                      | XRefStreamOptimization
                      | JPGOptimization
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
        else  return NoOptimization
    _notXMLorImage         -> getValue "Type" object >>= \case
      Just (PDFName "ObjStm") -> return ObjectStreamOptimization
      Just (PDFName "XRef") -> return XRefStreamOptimization
      _notObjectStream        -> if hasKey "Type" object
        then return NoOptimization
        else do
          tryF (getStream object) >>= \case
            Right stream -> case gfxParse stream of
              Right SQ.Empty -> return NoOptimization
              Right _        -> return GfxOptimization
              _notGfx        -> return NoOptimization
            _noStream -> return NoOptimization
