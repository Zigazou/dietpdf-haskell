{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
This modules implements several optimization techniques targeted at PDF objects.
-}
module Pdf.Object.Optimize
  ( optimize
  ) where

import           Codec.Compression.XML          ( optimizeXML )
import qualified Data.Map.Strict               as Map
import           Pdf.Object.Container           ( deepMap )
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFIndirectObject
                                                  , PDFIndirectObjectWithStream
                                                  , PDFName
                                                  , PDFObjectStream
                                                  , PDFTrailer
                                                  )
                                                , hasStream
                                                )
import           Pdf.Object.State               ( getStream
                                                , setStream
                                                )
import           Pdf.Object.String              ( optimizeString )
import           Pdf.Object.Format              ( txtObjectNumberVersion )
import           Pdf.Object.Unfilter            ( unfilter )
import           Pdf.Object.Filter              ( filterOptimize )
import           Util.Logging                   ( sayF
                                                , Logging
                                                )
import           Pdf.Graphics.Parser.Stream     ( gfxParse )
import           Pdf.Graphics.Object            ( separateGfx )
import qualified Data.Text                     as T
import           Util.UnifiedError              ( FallibleT
                                                , ifFail
                                                )

streamIsXML :: PDFObject -> Bool
streamIsXML (PDFIndirectObjectWithStream _ _ dictionary _) =
  dictionary Map.!? "Subtype" == Just (PDFName "XML")
streamIsXML _ = False

streamOptimize :: Logging m => PDFObject -> FallibleT m PDFObject
streamOptimize object = do
  stream <- getStream object
  if streamIsXML object
    then setStream (optimizeXML stream) object
    else case gfxParse stream of
      Right objects -> setStream (separateGfx objects) object
      _error        -> return object

{- |
Completely refilter a stream by finding the best filter combination.

It also optimized nested strings and XML streams.
-}
refilter :: Logging m => PDFObject -> FallibleT m PDFObject
refilter object = do
  sayF "  - Packing strings"
  stringOptimized <- deepMap optimizeString object

  if hasStream object
    then unfilter stringOptimized >>= streamOptimize >>= filterOptimize
    else return stringOptimized

{- |
Determine if a `PDFObject` is optimizable, whether because its filters are
known by DietPDF or because its structure is optimizable.
-}
optimizable :: PDFObject -> Bool
optimizable (PDFIndirectObjectWithStream _ _ dictionary _) =
  case Map.lookup "Filter" dictionary of
    Just (PDFName "FlateDecode") -> True
    Just (PDFName "RLEDecode"  ) -> True
    Just (PDFName "LZWDecode"  ) -> True
    Nothing                      -> True
    _anyOtherFilter              -> False
optimizable (PDFObjectStream _ _ dictionary _) =
  case Map.lookup "Filter" dictionary of
    Just (PDFName "FlateDecode") -> True
    Just (PDFName "RLEDecode"  ) -> True
    Just (PDFName "LZWDecode"  ) -> True
    Nothing                      -> True
    _anyOtherFilter              -> False
optimizable PDFIndirectObject{} = True
optimizable PDFTrailer{}        = True
optimizable _                   = False

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
optimize object = if optimizable object
  then do
    sayF (txtObjectNumberVersion object)
    refilter object
      `ifFail` (\anError -> do
                 sayF
                   ("  - Cannot optimize (" <> (T.pack . show) anError <> ")")
                 return object
               )
  else sayF (txtObjectNumberVersion object <> ": ignored") >> return object
