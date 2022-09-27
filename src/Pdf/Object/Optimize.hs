{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

{-|
This modules implements several optimization techniques targeted at PDF objects.
-}
module Pdf.Object.Optimize
  ( optimize
  ) where

import qualified Codec.Compression.Flate       as FL
import qualified Codec.Compression.RunLength   as RL
import           Codec.Compression.XML          ( optimizeXML )
import qualified Data.ByteString               as BS
import           Data.Foldable                  ( minimumBy )
import qualified Data.HashMap.Strict           as HM
import qualified Data.Sequence                 as SQ
import           Pdf.Object.Container           ( deepMap )
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFArray
                                                  , PDFIndirectObject
                                                  , PDFIndirectObjectWithStream
                                                  , PDFObjectStream
                                                  , PDFName
                                                  , PDFTrailer
                                                  )
                                                , updateStream
                                                )
import           Pdf.Object.String              ( optimizeString )
import           Util.Errors                    ( UnifiedError )
import           Pdf.Object.Unfilter            ( unfilter )

eMinOrder
  :: (a, Either b BS.ByteString) -> (a, Either b BS.ByteString) -> Ordering
eMinOrder (_, Right x) (_, Right y) = compare (BS.length x) (BS.length y)
eMinOrder (_, Right _) (_, Left _ ) = LT
eMinOrder (_, Left _ ) (_, Right _) = GT
eMinOrder _            _            = EQ

reduceArray :: [PDFObject] -> PDFObject
reduceArray [item] = item
reduceArray items  = PDFArray items

zopfli :: BS.ByteString -> ([PDFObject], Either UnifiedError BS.ByteString)
zopfli stream = ([PDFName "FlateDecode"], FL.compress stream)

rle :: BS.ByteString -> ([PDFObject], Either UnifiedError BS.ByteString)
rle stream = ([PDFName "RunLengthDecode"], RL.compress stream)

rleZopfli :: BS.ByteString -> ([PDFObject], Either UnifiedError BS.ByteString)
rleZopfli stream =
  ( [PDFName "FlateDecode", PDFName "RunLengthDecode"]
  , snd (rle stream) >>= FL.compress
  )

filterOptimize :: PDFObject -> Either UnifiedError PDFObject
filterOptimize (PDFIndirectObjectWithStream num gen dict stream) = do
  let (bestFilters, bestStream) = minimumBy
        eMinOrder
        (SQ.fromList [zopfli stream, rle stream, rleZopfli stream])
  PDFIndirectObjectWithStream
      num
      gen
      (HM.insert "Filter" (reduceArray bestFilters) dict)
    <$> bestStream
filterOptimize (PDFObjectStream num gen dict stream) = do
  let (bestFilters, bestStream) = minimumBy
        eMinOrder
        (SQ.fromList [zopfli stream, rle stream, rleZopfli stream])
  PDFObjectStream num gen (HM.insert "Filter" (reduceArray bestFilters) dict)
    <$> bestStream

filterOptimize object = return object

streamIsXML :: PDFObject -> Bool
streamIsXML (PDFIndirectObjectWithStream _ _ dictionary _) =
  case dictionary HM.!? "Subtype" of
    Just (PDFName "XML") -> True
    _anyOtherValue       -> False
streamIsXML _ = False

streamOptimize :: PDFObject -> Either UnifiedError PDFObject
streamOptimize object@(PDFIndirectObjectWithStream _ _ _ stream)
  | streamIsXML object = return $ updateStream object (optimizeXML stream)
  | otherwise          = return object
streamOptimize object@(PDFObjectStream _ _ _ stream)
  | streamIsXML object = return $ updateStream object (optimizeXML stream)
  | otherwise          = return object
streamOptimize object = return object

{- |
Completely refilter a stream by finding the best filter combination.

It also optimized nested strings and XML streams.
-}
refilter :: PDFObject -> Either UnifiedError PDFObject
refilter object@PDFIndirectObjectWithStream{} =
  unfilter (deepMap optimizeString object) >>= streamOptimize >>= filterOptimize
refilter object@PDFObjectStream{} =
  unfilter (deepMap optimizeString object) >>= streamOptimize >>= filterOptimize
refilter object = return (deepMap optimizeString object)

{- |
Determine if a `PDFObject` is optimizable, whether because its filters are
known by DietPDF or because its structure is optimizable.
-}
optimizable :: PDFObject -> Bool
optimizable (PDFIndirectObjectWithStream _ _ dictionary _) =
  case HM.lookup "Filter" dictionary of
    Just (PDFName "FlateDecode") -> True
    Just (PDFName "RLEDecode"  ) -> True
    Just (PDFName "LZWDecode"  ) -> True
    Nothing                      -> True
    _anyOtherFilter              -> False
optimizable (PDFObjectStream _ _ dictionary _) =
  case HM.lookup "Filter" dictionary of
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
optimize :: PDFObject -> PDFObject
optimize object
  | optimizable object = case refilter object of
    Right optimizedObject -> optimizedObject
    Left  _               -> object
  | otherwise = object
