{-|
This modules implements several optimization techniques targeted at PDF objects.
-}
module Pdf.Object.Optimize
  ( optimize
  ) where

import Codec.Compression.XML (optimizeXML)

import Data.ByteString qualified as BS
import Data.Context (Contextual (ctx))
import Data.Logging (Logging, sayComparisonF, sayErrorF, sayF)
import Data.Sequence qualified as SQ
import Data.UnifiedError (FallibleT, ifFail)

import External.JpegTran (jpegtranOptimize)
import External.TtfAutoHint (ttfAutoHintOptimize)

import Pdf.Graphics.Optimize (optimizeGFX)
import Pdf.Object.Container (Filter (fFilter), deepMap, getFilters)
import Pdf.Object.Filter (filterOptimize)
import Pdf.Object.Object
    ( PDFObject (PDFIndirectObject, PDFIndirectObjectWithStream, PDFName, PDFObjectStream, PDFTrailer, PDFXRefStream)
    , hasStream
    )
import Pdf.Object.OptimizationType
    ( OptimizationType (GfxOptimization, JPGOptimization, TTFOptimization, XMLOptimization)
    , whatOptimizationFor
    )
import Pdf.Object.State (getStream, setStream, setStream1)
import Pdf.Object.String (optimizeString)
import Pdf.Object.Unfilter (unfilter)

streamOptimize :: Logging IO => PDFObject -> FallibleT IO PDFObject
streamOptimize object =
  let context = ctx object
  in whatOptimizationFor object >>= \case
    XMLOptimization -> do
      stream <- getStream object
      let optimizedStream = optimizeXML stream
      sayComparisonF context
                    "XML stream optimization"
                    (BS.length stream)
                    (BS.length optimizedStream)
      setStream optimizedStream object

    GfxOptimization -> do
      stream <- getStream object >>= optimizeGFX context
      setStream stream object

    JPGOptimization -> do
      stream <- getStream object
      optimizedStream <- jpegtranOptimize stream
      sayComparisonF context
                    "JPG stream optimization"
                    (BS.length stream)
                    (BS.length optimizedStream)

      setStream optimizedStream object

    TTFOptimization -> do
      stream <- getStream object
      optimizedStream <- ttfAutoHintOptimize stream
      sayComparisonF context
                    "TTF stream optimization"
                    (BS.length stream)
                    (BS.length optimizedStream)

      setStream1 (BS.length optimizedStream) optimizedStream object

    _anyOtherOptimization -> return object

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
      unfilter stringOptimized
        >>= streamOptimize
        >>= filterOptimize optimization
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
  True -> refilter object
    `ifFail` (\theError -> sayErrorF (ctx object) "cannot optimize" theError
                        >> return object
             )
  False -> do
    sayF (ctx object) "ignored"
    return object
