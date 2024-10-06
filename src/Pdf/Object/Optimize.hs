{-|
This modules implements several optimization techniques targeted at PDF objects.
-}
module Pdf.Object.Optimize
  ( optimize
  ) where

import Codec.Compression.XML (optimizeXML)

import Control.Monad.Trans.Except (except)

import Data.ByteString qualified as BS
import Data.Context (Contextual (ctx))
import Data.Fallible (FallibleT, ifFail, tryF)
import Data.Logging (Logging, sayComparisonF, sayErrorF, sayF)
import Data.Sequence qualified as SQ
import Data.Text qualified as T
import Data.TranslationTable (TranslationTable)

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


optimizeStreamOrIgnore
  :: Logging m
  => T.Text
  -> PDFObject
  -> (BS.ByteString -> FallibleT m BS.ByteString)
  -> FallibleT m BS.ByteString
optimizeStreamOrIgnore optimizationLabel object optimizationProcess = do
  let context = ctx object
  stream <- getStream object
  tryF (optimizationProcess stream) >>= \case
    Right optimizedStream -> do
      sayComparisonF context
                     optimizationLabel
                     (BS.length stream)
                     (BS.length optimizedStream)
      return optimizedStream
    Left anError -> do
      sayErrorF context "cannot optimize" anError
      return stream

streamOptimize
  :: Logging IO
  => TranslationTable
  -> PDFObject
  -> FallibleT IO PDFObject
streamOptimize nameTranslations object =
  let context = ctx object
  in whatOptimizationFor object >>= \case
    XMLOptimization -> do
      optimizedStream <- optimizeStreamOrIgnore "XML stream optimization"
                                                object
                                                (except . optimizeXML)
      setStream optimizedStream object

    GfxOptimization -> do
      stream <- getStream object >>= optimizeGFX context nameTranslations
      setStream stream object

    JPGOptimization -> do
      optimizedStream <- optimizeStreamOrIgnore "JPG stream optimization"
                                                object
                                                jpegtranOptimize
      setStream optimizedStream object

    TTFOptimization -> do
      optimizedStream <- optimizeStreamOrIgnore "TTF stream optimization"
                                                object
                                                ttfAutoHintOptimize
      setStream1 (BS.length optimizedStream) optimizedStream object

    _anyOtherOptimization -> return object

{- |
Completely refilter a stream by finding the best filter combination.

It also optimized nested strings and XML streams.
-}
refilter
  :: Logging IO
  => TranslationTable
  -> PDFObject
  -> FallibleT IO PDFObject
refilter nameTranslations object = do
  stringOptimized <- deepMap optimizeString object

  if hasStream object
    then do
      optimization <- whatOptimizationFor object
      unfilter stringOptimized
        >>= streamOptimize nameTranslations
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
optimize
  :: Logging IO
  => TranslationTable
  -> PDFObject
  -> FallibleT IO PDFObject
optimize nameTranslations object = optimizable object >>= \case
  True -> refilter nameTranslations object
    `ifFail` (\theError -> sayErrorF (ctx object) "cannot optimize" theError
                        >> return object
             )
  False -> do
    sayF (ctx object) "ignored"
    return object
