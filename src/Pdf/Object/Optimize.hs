{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StrictData #-}

{-|
This modules implements several optimization techniques targeted at PDF objects.
-}
module Pdf.Object.Optimize
  ( optimize
  ) where

import qualified Codec.Compression.Flate       as FL
import qualified Codec.Compression.LZW         as LZ
import qualified Codec.Compression.RunLength   as RL
import           Codec.Compression.XML          ( optimizeXML )
import qualified Data.ByteString               as BS
import           Data.Foldable                  ( minimumBy )
import qualified Data.HashMap.Strict           as HM
import qualified Data.Sequence                 as SQ
import           Pdf.Object.Container           ( deepMap )
import           Pdf.Object.Object              ( Dictionary
                                                , PDFObject
                                                  ( PDFArray
                                                  , PDFDictionary
                                                  , PDFIndirectObject
                                                  , PDFName
                                                  , PDFNull
                                                  , PDFTrailer
                                                  )
                                                , updateStream
                                                )
import           Pdf.Object.String              ( optimizeString )
import           Util.Errors                    ( UnifiedError
                                                  ( InvalidFilterParm
                                                  )
                                                )

data Filter = Filter
  { fFilter      :: PDFObject
  , fDecodeParms :: PDFObject
  }

hasNoDecodeParms :: Filter -> Bool
hasNoDecodeParms = (== PDFNull) . fDecodeParms

unfilterStream
  :: ([Filter], BS.ByteString) -> Either UnifiedError ([Filter], BS.ByteString)
unfilterStream (filters@(pdfFilter : otherFilters), stream)
  | fFilter pdfFilter == PDFName "FlateDecode"
  = FL.decompress stream >>= unfilterStream . (otherFilters, )
  | fFilter pdfFilter == PDFName "RLEDecode"
  = RL.decompress stream >>= unfilterStream . (otherFilters, )
  | fFilter pdfFilter == PDFName "LZWDecode"
  = LZ.decompress stream >>= unfilterStream . (otherFilters, )
  | otherwise
  = Right (filters, stream)
unfilterStream (filters, stream) = Right (filters, stream)

getFilters :: Dictionary -> Either UnifiedError [Filter]
getFilters dict =
  case (HM.lookup "Filter" dict, HM.lookup "DecodeParms" dict) of
    (Just (  PDFArray fs), Just PDFNull      ) -> return $ group fs []
    (Just (  PDFArray fs), Nothing           ) -> return $ group fs []
    (Just (  PDFArray fs), Just (PDFArray ps)) -> return $ group fs ps
    (Just (  PDFArray fs), Just object       ) -> return $ group fs [object]

    (Just f@(PDFName  _ ), Just PDFNull      ) -> return $ group [f] []
    (Just f@(PDFName  _ ), Nothing           ) -> return $ group [f] []
    (Just f@(PDFName  _ ), Just (PDFArray ps)) -> return $ group [f] ps
    (Just f@(PDFName  _ ), Just p            ) -> return $ group [f] [p]

    (Nothing             , _                 ) -> return []
    (_                   , _                 ) -> Left InvalidFilterParm
 where
  group :: [PDFObject] -> [PDFObject] -> [Filter]
  group fs ps = zipWith Filter fs (ps ++ repeat PDFNull)
getFilters _ = return []

setFilters :: [Filter] -> Maybe PDFObject
setFilters []                           = Nothing
setFilters [Filter aName@(PDFName _) _] = Just aName
setFilters filters                      = Just (PDFArray $ fFilter <$> filters)

setDecodeParms :: [Filter] -> Maybe PDFObject
setDecodeParms []                      = Nothing
setDecodeParms [Filter _ PDFNull     ] = Nothing
setDecodeParms [Filter _ aDecodeParms] = Just aDecodeParms
setDecodeParms filters | all hasNoDecodeParms filters = Nothing
                       | otherwise = Just (PDFArray $ fDecodeParms <$> filters)

insertMaybe :: Dictionary -> BS.ByteString -> Maybe PDFObject -> Dictionary
insertMaybe dict name (Just object) = HM.insert name object dict
insertMaybe dict _    Nothing       = dict

insertMaybes :: Dictionary -> [(BS.ByteString, Maybe PDFObject)] -> Dictionary
insertMaybes dict [] = dict
insertMaybes dict ((name, value) : remains) =
  insertMaybes (insertMaybe dict name value) remains

unfilter :: PDFObject -> Either UnifiedError PDFObject
unfilter (PDFIndirectObject num gen (PDFDictionary dict) (Just stream)) = do
  (remainingFilters, unfilteredStream) <- unfiltered
  return $ PDFIndirectObject
    num
    gen
    ( PDFDictionary
    $ (insertMaybes
        dict
        [ ("DecodeParms", setDecodeParms remainingFilters)
        , ("Filter"     , setFilters remainingFilters)
        ]
      )
    )
    (Just unfilteredStream)
 where
  unfiltered :: Either UnifiedError ([Filter], BS.ByteString)
  unfiltered = getFilters dict >>= \filters -> unfilterStream (filters, stream)
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

{- |
Completely refilter a stream by finding the best filter combination.

It also optimized nested strings and XML streams.
-}
refilter :: PDFObject -> Either UnifiedError PDFObject
refilter object@(PDFIndirectObject _ _ (PDFDictionary _) (Just _)) =
  unfilter (deepMap optimizeString object) >>= streamOptimize >>= filterOptimize
refilter object = return (deepMap optimizeString object)

{- |
Determine if a `PDFObject` is optimizable, whether because its filters are
known by DietPDF or because its structure is optimizable.
-}
optimizable :: PDFObject -> Bool
optimizable (PDFIndirectObject _ _ (PDFDictionary dictionary) (Just _)) =
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
