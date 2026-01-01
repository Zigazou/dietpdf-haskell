{- |
Optimization passes for PDF objects and their streams.

This module decides which optimization to apply based on object type and
filters, performs safe unfiltering, stream-level optimizations (XML, graphics,
JPEG, TrueType), and then re-filters using suitable combinations. It integrates
with the `PDFWork` monad to report progress and errors with contextual
information.
-}

module PDF.Processing.Optimize
  ( optimize
  ) where

import Codec.Compression.XML (optimizeXML)

import Control.Monad.State (lift)

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Context (Contextual (ctx))
import Data.Logging (Logging)
import Data.PDF.Filter (Filter (fFilter))
import Data.PDF.OptimizationType
    ( OptimizationType (GfxOptimization, JPGOptimization, TTFOptimization, XMLOptimization)
    )
import Data.PDF.PDFObject
    ( PDFObject (PDFIndirectObject, PDFIndirectObjectWithStream, PDFName, PDFObjectStream, PDFTrailer, PDFXRefStream)
    , hasStream
    )
import Data.PDF.PDFWork
    ( PDFWork
    , sayComparisonP
    , sayErrorP
    , sayP
    , tryP
    , withContext
    )
import Data.Sequence qualified as SQ
import Data.Text qualified as T

import External.JpegTran (jpegtranOptimize)
import External.TtfAutoHint (ttfAutoHintOptimize)

import PDF.Graphics.Optimize (optimizeGFX)
import PDF.Object.Container (getFilters)
import PDF.Object.State (getStream, setStream, setStream1)
import PDF.Object.String (optimizeString)
import PDF.Processing.Filter (filterOptimize)
import PDF.Processing.PDFWork (deepMapP)
import PDF.Processing.Unfilter (unfilter)
import PDF.Processing.WhatOptimizationFor (whatOptimizationFor)


{- |
Run a stream optimization process for a given `PDFObject`.

If the optimization succeeds, logs a size comparison and returns the optimized
stream; on failure, logs the error and returns the original stream.
-}
optimizeStreamOrIgnore
  :: Logging m
  => T.Text
  -> PDFObject
  -> (ByteString -> PDFWork m ByteString)
  -> PDFWork m ByteString
optimizeStreamOrIgnore optimizationLabel object optimizationProcess = do
  stream <- getStream object
  tryP (optimizationProcess stream) >>= \case
    Right optimizedStream -> do
      sayComparisonP optimizationLabel
                     (BS.length stream)
                     (BS.length optimizedStream)
      return optimizedStream
    Left anError -> do
      sayErrorP "cannot optimize" anError
      return stream

{- |
Apply an appropriate stream-level optimization based on the object content (XML,
graphics, JPEG, TrueType font). Returns the object with its stream updated when
applicable.
-}
streamOptimize :: PDFObject -> PDFWork IO PDFObject
streamOptimize object = do
  whatOptimizationFor object >>= \case
    XMLOptimization -> do
      optimizedStream <- optimizeStreamOrIgnore "XML stream optimization"
                                                object
                                                optimizeXML
      setStream optimizedStream object

    GfxOptimization -> do
      getStream object >>= optimizeGFX >>= flip setStream object

    JPGOptimization -> do
      optimizedStream <- optimizeStreamOrIgnore "JPG stream optimization"
                                                object
                                                (lift . jpegtranOptimize)
      setStream optimizedStream object

    TTFOptimization -> do
      optimizedStream <- optimizeStreamOrIgnore "TTF stream optimization"
                                                object
                                                (lift . ttfAutoHintOptimize)
      setStream1 (BS.length optimizedStream) optimizedStream object

    _anyOtherOptimization -> return object

{- |
Completely refilter a stream by finding the best filter combination.

It also optimized nested strings and XML streams.
-}
refilter :: PDFObject -> PDFWork IO PDFObject
refilter object = do
  stringOptimized <- deepMapP optimizeString object

  if hasStream object
    then do
      optimization <- whatOptimizationFor object
      unfilter stringOptimized
        >>= streamOptimize
        >>= filterOptimize optimization
    else return stringOptimized

{- |
Return whether a filter is known and supported for optimization.
-}
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
optimizable :: Logging m => PDFObject -> PDFWork m Bool
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
- optimizing JPG images
- optimizing TTF fonts
- optimizing graphics streams

Optimization of spaces is done at the `PDFObject` level, not by this function.

If the PDF object is not elligible to optimization or if optimization is
ineffective, it is returned as is.
-}
optimize :: PDFObject -> PDFWork IO PDFObject
optimize object = withContext (ctx ("optimize" :: String) <> ctx object) $ do
  objectCanBeOptimized <- optimizable object

  if objectCanBeOptimized
    then
      -- refilter object
      tryP (refilter object) >>= \case
       Right optimizedObject -> return optimizedObject
       Left  theError         -> do
         sayErrorP "cannot optimize" theError
         return object
    else do
      sayP "ignored"
      return object
