{-|
This modules implements several optimization techniques targeted at PDF objects.
-}
module Pdf.Object.Optimize
  ( optimize
  ) where

import Codec.Compression.XML (optimizeXML)

import Data.ByteString qualified as BS
import Data.Kind (Type)
import Data.Sequence qualified as SQ

import External.JpegTran (jpegtranOptimize)

import Pdf.Graphics.Optimize (optimizeGFX)
import Pdf.Graphics.Parser.Stream (gfxParse)
import Pdf.Object.Container (Filter (fFilter), deepMap, getFilters)
import Pdf.Object.Filter (filterOptimize)
import Pdf.Object.Format (txtObjectNumberVersion)
import Pdf.Object.Object
    ( PDFObject (PDFIndirectObject, PDFIndirectObjectWithStream, PDFName, PDFObjectStream, PDFTrailer, PDFXRefStream)
    , hasKey
    , hasStream
    )
import Pdf.Object.State (getStream, getValue, setStream)
import Pdf.Object.String (optimizeString)
import Pdf.Object.Unfilter (unfilter)

import Util.Logging (Logging, sayComparisonF, sayErrorF, sayF)
import Util.UnifiedError (FallibleT, ifFail, tryF)

type OptimizationType :: Type
data OptimizationType = XMLOptimization
                      | GfxOptimization
                      | ObjectStreamOptimization
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
      _notObjectStream        -> if hasKey "Type" object
        then return NoOptimization
        else do
          tryF (getStream object) >>= \case
            Right stream -> case gfxParse stream of
              Right SQ.Empty -> return NoOptimization
              Right _        -> return GfxOptimization
              _notGfx        -> return NoOptimization
            _noStream -> return NoOptimization

streamOptimize :: Logging IO => PDFObject -> FallibleT IO PDFObject
streamOptimize object = whatOptimizationFor object >>= \case
  XMLOptimization -> do
    stream <- getStream object
    let optimizedStream = optimizeXML stream
    sayComparisonF "XML stream optimization"
                   (BS.length stream)
                   (BS.length optimizedStream)
    setStream optimizedStream object

  ObjectStreamOptimization -> return object

  GfxOptimization -> do
    stream <- getStream object >>= optimizeGFX
    setStream stream object

  JPGOptimization -> do
    stream <- getStream object
    optimizedStream <- jpegtranOptimize stream
    sayComparisonF "JPG stream optimization"
                (BS.length stream)
                (BS.length optimizedStream)

    setStream stream object

  NoOptimization -> return object

{- |
Completely refilter a stream by finding the best filter combination.

It also optimized nested strings and XML streams.
-}
refilter :: Logging IO => PDFObject -> FallibleT IO PDFObject
refilter object = do
  stringOptimized <- deepMap optimizeString object

  if hasStream object
    then do
      optimization <- whatOptimizationFor object
      if optimization == JPGOptimization
        then unfilter stringOptimized >>= streamOptimize
        else unfilter stringOptimized >>= streamOptimize >>= filterOptimize
    else return stringOptimized

isFilterOK :: Filter -> Bool
isFilterOK f = case fFilter f of
  (PDFName "FlateDecode"   ) -> True
  (PDFName "RLEDecode"     ) -> True
  (PDFName "LZWDecode"     ) -> True
  (PDFName "ASCII85Decode" ) -> True
  (PDFName "ASCIIHexDecode") -> True
  (PDFName "DCTDecode"     ) -> True
  (PDFName "JPXDecode"     ) -> True
  _anyOtherCase              -> False

{- |
Determine if a `PDFObject` is optimizable, whether because its filters are
known by DietPDF or because its structure is optimizable.
-}
optimizable :: Logging m => PDFObject -> FallibleT m Bool
optimizable PDFIndirectObject{}                  = return True
optimizable PDFTrailer{}                         = return True
optimizable PDFXRefStream{}                      = return True
optimizable object@PDFIndirectObjectWithStream{} = do
  filters <- getFilters object
  let unsupportedFilters = SQ.filter (not . isFilterOK) filters
  return $ SQ.null unsupportedFilters
optimizable object@PDFObjectStream{} = do
  filters <- getFilters object
  let unsupportedFilters = SQ.filter (not . isFilterOK) filters
  return $ SQ.null unsupportedFilters
optimizable _anyOtherObject = return False

{- |
Optimize a PDF object.

`PDFObject` may be optimized by:

- using Zopfli instead of Zlib
- combining Zopfli and RLE
- removing unneeded spaces in XML stream

Optimization of spaces is done at the `PDFObject` level, not by this function.

If the PDF object is not elligible to optimization or if optimization is
ineffective, it is returned as is.
-}
{-# INLINE optimize #-}
optimize :: Logging IO => PDFObject -> FallibleT IO PDFObject
optimize object = optimizable object >>= \case
  True -> do
    sayF (txtObjectNumberVersion object)
    ifFail (refilter object)
           (\theError -> sayErrorF "Cannot optimize" theError >> return object)
  False -> do
    sayF (txtObjectNumberVersion object <> ": ignored")
    return object
