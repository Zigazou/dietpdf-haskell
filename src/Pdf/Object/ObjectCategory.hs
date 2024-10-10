module Pdf.Object.ObjectCategory (objectCategory) where
import Data.Fallible (FallibleT, tryF)
import Data.Logging (Logging)
import Data.Map qualified as Map
import Data.ObjectCategory
    ( ObjectCategory (Bitmap, File, Font, Other, Vector, XML)
    )

import Pdf.Object.Object (PDFObject (PDFName))
import Pdf.Object.OptimizationType
    ( OptimizationType (GfxOptimization, JPGOptimization, TTFOptimization, XMLOptimization)
    , whatOptimizationFor
    )
import Pdf.Object.State (getDictionary)
import Pdf.Object.Unfilter (unfilter)

objectCategory :: Logging IO => PDFObject -> FallibleT IO ObjectCategory
objectCategory object = tryF (getDictionary object) >>= \case
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
        _anyOtherType           -> return Other
