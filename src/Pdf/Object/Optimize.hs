{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{-|
This modules implements several optimization techniques targeted at PDF objects.
-}
module Pdf.Object.Optimize
  ( optimize
  ) where

import qualified Data.HashMap.Strict           as HM
import qualified Codec.Compression.Flate       as FL
import qualified Codec.Compression.RunLength   as RL
import qualified Codec.Compression.LZW         as LZ
import           Codec.Compression.XML          ( optimizeXML )
import qualified Data.ByteString               as BS
import           Data.Foldable                  ( minimumBy )
import qualified Data.Sequence                 as SQ
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFIndirectObject
                                                  , PDFDictionary
                                                  , PDFName
                                                  , PDFArray
                                                  , PDFTrailer
                                                  )
                                                , updateStream
                                                )
import           Pdf.Object.String              ( optimizeString )
import           Pdf.Object.Container           ( deepMap )
import           Util.Errors                    ( UnifiedError )

unfilterStream
  :: ([PDFObject], BS.ByteString)
  -> Either UnifiedError ([PDFObject], BS.ByteString)
unfilterStream (filters@(pdfFilter : otherFilters), stream)
  | pdfFilter == PDFName "FlateDecode"
  = FL.decompress stream >>= unfilterStream . (otherFilters, )
  | pdfFilter == PDFName "RLEDecode"
  = RL.decompress stream >>= unfilterStream . (otherFilters, )
  | pdfFilter == PDFName "LZWDecode"
  = LZ.decompress stream >>= unfilterStream . (otherFilters, )
  | otherwise
  = Right (filters, stream)
unfilterStream (filters, stream) = Right (filters, stream)

unfilter :: PDFObject -> Either UnifiedError PDFObject
unfilter (PDFIndirectObject num gen (PDFDictionary dict) (Just stream)) = do
  (remainingFilters, unfilteredStream) <- unfiltered
  return $ PDFIndirectObject
    num
    gen
    (PDFDictionary $ HM.insert "Filter" (PDFArray remainingFilters) dict)
    (Just unfilteredStream)
 where
  unfiltered :: Either UnifiedError ([PDFObject], BS.ByteString)
  unfiltered = unfilterStream $ case HM.lookup "Filter" dict of
    Just aName@(PDFName  _      ) -> ([aName], stream)
    Just (      PDFArray filters) -> (filters, stream)
    _anyOtherSettings             -> ([], stream)
unfilter object = return object

eMinOrder
  :: (a, Either b BS.ByteString) -> (a, Either b BS.ByteString) -> Ordering
eMinOrder (_, Right x) (_, Right y) = compare (BS.length x) (BS.length y)
eMinOrder (_, Right _) (_, Left _ ) = LT
eMinOrder (_, Left _ ) (_, Right _) = GT
eMinOrder _            _            = EQ

reduceArray :: [PDFObject] -> PDFObject
reduceArray [item] = item
reduceArray items  = PDFArray items

filterOptimize :: PDFObject -> Either UnifiedError PDFObject
filterOptimize (PDFIndirectObject num gen (PDFDictionary dict) (Just stream)) =
  do
    let (bestFilters, bestStream) =
          minimumBy eMinOrder (SQ.fromList [zopfli, rle, rleZopfli])
    PDFIndirectObject
        num
        gen
        (PDFDictionary $ HM.insert "Filter" (reduceArray bestFilters) dict)
      .   Just
      <$> bestStream
 where
  zopfli :: ([PDFObject], Either UnifiedError BS.ByteString)
  zopfli = ([PDFName "FlateDecode"], FL.compress stream)

  rle :: ([PDFObject], Either UnifiedError BS.ByteString)
  rle = ([PDFName "RunLengthDecode"], RL.compress stream)

  rleZopfli :: ([PDFObject], Either UnifiedError BS.ByteString)
  rleZopfli =
    ( [PDFName "FlateDecode", PDFName "RunLengthDecode"]
    , snd rle >>= FL.compress
    )

filterOptimize object = return object

streamIsXML :: PDFObject -> Bool
streamIsXML (PDFIndirectObject _ _ (PDFDictionary dictionary) (Just _)) =
  case dictionary HM.!? "Subtype" of
    Just (PDFName "XML") -> True
    _anyOtherValue       -> False
streamIsXML _ = False

streamOptimize :: PDFObject -> Either UnifiedError PDFObject
streamOptimize object@(PDFIndirectObject _ _ (PDFDictionary _) (Just stream))
  | streamIsXML object = return $ updateStream object (optimizeXML stream)
  | otherwise          = return object
streamOptimize object = return object

refilter :: PDFObject -> Either UnifiedError PDFObject
refilter object@(PDFIndirectObject _ _ (PDFDictionary _) (Just _)) =
  unfilter (deepMap optimizeString object) >>= streamOptimize >>= filterOptimize
refilter object = return (deepMap optimizeString object)

optimizable :: PDFObject -> Bool
optimizable (PDFIndirectObject _ _ (PDFDictionary dictionary) (Just _)) =
  case HM.lookup "Filter" dictionary of
    Just (PDFName "FlateDecode") -> True
    Just (PDFName "RLEDecode"  ) -> True
    Just (PDFName "LZWDecode"  ) -> True
    Nothing                      -> True
    _anyOtherFilter              -> False
optimizable PDFIndirectObject {} = True
optimizable PDFTrailer {} = True
optimizable _ = False

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
