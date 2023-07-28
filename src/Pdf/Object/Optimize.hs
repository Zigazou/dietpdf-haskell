{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

{-|
This modules implements several optimization techniques targeted at PDF objects.
-}
module Pdf.Object.Optimize
  ( optimize
  ) where

import           Codec.Compression.XML          ( optimizeXML )
import           Pdf.Object.Container           ( deepMap
                                                , Filter(fFilter)
                                                , getFilters
                                                )
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFIndirectObject
                                                  , PDFIndirectObjectWithStream
                                                  , PDFName
                                                  , PDFObjectStream
                                                  , PDFTrailer
                                                  )
                                                , hasStream
                                                , hasKey
                                                )
import           Pdf.Object.State               ( getStream
                                                , setStream
                                                , getValue
                                                )
import           Pdf.Object.String              ( optimizeString )
import           Pdf.Object.Format              ( txtObjectNumberVersion )
import           Pdf.Object.Unfilter            ( unfilter )
import           Pdf.Object.Filter              ( filterOptimize )
import           Util.Logging                   ( sayF
                                                , Logging
                                                , sayComparisonF
                                                , sayErrorF
                                                )
import           Pdf.Graphics.Parser.Stream     ( gfxParse )
import           Pdf.Graphics.Object            ( separateGfx )
import           Util.UnifiedError              ( FallibleT
                                                , ifFail
                                                , tryF
                                                )
import qualified Data.ByteString               as BS
import qualified Data.Sequence                 as SQ

data OptimizationType = XMLOptimization
                      | GfxOptimization
                      | ObjectStreamOptimization
                      | NoOptimization
                      deriving stock (Eq)

whatOptimizationFor :: Logging m => PDFObject -> FallibleT m OptimizationType
whatOptimizationFor object = do
  getValue "SubType" object >>= \case
    Just (PDFName "XML") -> return XMLOptimization
    _notXML              -> getValue "Type" object >>= \case
      Just (PDFName "ObjStm") -> return ObjectStreamOptimization
      _notObjectStream        -> if hasKey "Type" object
        then return NoOptimization
        else do
          tryF (getStream object) >>= \case
            Right stream -> case gfxParse stream of
              Right _ -> return GfxOptimization
              _notGfx -> return NoOptimization
            _noStream -> return NoOptimization

streamOptimize :: Logging m => PDFObject -> FallibleT m PDFObject
streamOptimize object = whatOptimizationFor object >>= \case
  XMLOptimization -> do
    stream <- getStream object
    let optimizedStream = optimizeXML stream
    sayComparisonF "XML stream optimization"
                   (BS.length stream)
                   (BS.length optimizedStream)
    setStream optimizedStream object
  ObjectStreamOptimization -> do
    sayF "  - No optimization for object stream"
    return object
  GfxOptimization -> do
    stream <- getStream object
    case gfxParse stream of
      Right objects -> do
        let optimizedStream = separateGfx objects
        sayComparisonF "GFX objects optimization"
                       (BS.length stream)
                       (BS.length optimizedStream)
        setStream optimizedStream object
      _error -> do
        sayF "  - No stream content to optimize"
        return object
  NoOptimization -> do
    sayF "  - No stream content to optimize"
    return object

{- |
Completely refilter a stream by finding the best filter combination.

It also optimized nested strings and XML streams.
-}
refilter :: Logging m => PDFObject -> FallibleT m PDFObject
refilter object = do
  stringOptimized <- deepMap optimizeString object

  if hasStream object
    then unfilter stringOptimized >>= streamOptimize >>= filterOptimize
    else return stringOptimized

isFilterOK :: Filter -> Bool
isFilterOK f = case fFilter f of
  (PDFName "FlateDecode"   ) -> True
  (PDFName "RLEDecode"     ) -> True
  (PDFName "LZWDecode"     ) -> True
  (PDFName "ASCII85Decode" ) -> True
  (PDFName "ASCIIHexDecode") -> True
  _anyOtherCase              -> False

{- |
Determine if a `PDFObject` is optimizable, whether because its filters are
known by DietPDF or because its structure is optimizable.
-}
optimizable :: Logging m => PDFObject -> FallibleT m Bool
optimizable PDFIndirectObject{}                  = return True
optimizable PDFTrailer{}                         = return True
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
optimize :: Logging m => PDFObject -> FallibleT m PDFObject
optimize object = optimizable object >>= \case
  True -> do
    sayF (txtObjectNumberVersion object)
    refilter object
      `ifFail` (\theError -> do
                 sayErrorF "Cannot optimize" theError
                 return object
               )
  False -> do
    sayF (txtObjectNumberVersion object <> ": ignored")
    return object
