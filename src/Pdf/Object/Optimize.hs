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
import qualified Codec.Filter.Ascii85          as A8
import qualified Codec.Filter.AsciiHex         as AH
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
import           Util.Step                      ( StepM
                                                , step
                                                , action
                                                )
import qualified Data.Text                     as T
import           Text.Printf                    ( printf )

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
  | fFilter pdfFilter == PDFName "RunLengthDecode"
  = RL.decompress stream >>= unfilterStream . (otherFilters, )
  | fFilter pdfFilter == PDFName "LZWDecode"
  = LZ.decompress stream >>= unfilterStream . (otherFilters, )
  | fFilter pdfFilter == PDFName "ASCII85Decode"
  = A8.decode stream >>= unfilterStream . (otherFilters, )
  | fFilter pdfFilter == PDFName "ASCIIHexDecode"
  = AH.decode stream >>= unfilterStream . (otherFilters, )
  | otherwise
  = Right (filters, stream)
unfilterStream (filters, stream) = Right (filters, stream)

getFilters :: Dictionary -> Either UnifiedError [Filter]
getFilters dict =
  case (HM.lookup "Filter" dict, HM.lookup "DecodeParms" dict) of
    (Just (  PDFArray fs), Just PDFNull      ) -> group fs []
    (Just (  PDFArray fs), Nothing           ) -> group fs []
    (Just (  PDFArray fs), Just (PDFArray ps)) -> group fs ps
    (Just (  PDFArray fs), Just object       ) -> group fs [object]

    (Just f@(PDFName  _ ), Just PDFNull      ) -> group [f] []
    (Just f@(PDFName  _ ), Nothing           ) -> group [f] []
    (Just f@(PDFName  _ ), Just (PDFArray ps)) -> group [f] ps
    (Just f@(PDFName  _ ), Just p            ) -> group [f] [p]

    (Nothing             , _                 ) -> return []
    (_                   , _                 ) -> Left InvalidFilterParm
 where
  group :: [PDFObject] -> [PDFObject] -> Either UnifiedError [Filter]
  group fs ps = return $ zipWith Filter fs (ps ++ repeat PDFNull)

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
    (PDFDictionary $ insertMaybes
      dict
      [ ("DecodeParms", setDecodeParms remainingFilters)
      , ("Filter"     , setFilters remainingFilters)
      ]
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

sizeComparison
  :: BS.ByteString -> ([PDFObject], Either UnifiedError BS.ByteString) -> T.Text
sizeComparison before (_, Right after) = T.pack
  $ printf "%d/%d (%+.2f%%)" sizeBefore sizeAfter ratio
 where
  sizeBefore = BS.length before
  sizeAfter  = BS.length after
  ratio :: Float
  ratio =
    100
      * (fromIntegral sizeAfter - fromIntegral sizeBefore)
      / fromIntegral sizeBefore

sizeComparison _ (_, Left _) = "error"

filterOptimize :: StepM m => PDFObject -> m (Either UnifiedError PDFObject)
filterOptimize object@(PDFIndirectObject num gen (PDFDictionary dict) (Just stream))
  = do
    step ("Optimizing filters of object " <> objectNumberText object)

    zopfli <- do
      let result     = ([PDFName "FlateDecode"], FL.compress stream)
          comparison = sizeComparison stream result
      step ("Evaluating Deflate with Zopfli: " <> comparison)
      return result

    rle <- do
      let result     = ([PDFName "RunLengthDecode"], RL.compress stream)
          comparison = sizeComparison stream result
      step ("Evaluating RLE: " <> comparison)
      return result

    rleZopfli <- do
      let result =
            ( [PDFName "FlateDecode", PDFName "RunLengthDecode"]
            , snd rle >>= FL.compress
            )
          comparison = sizeComparison stream result
      step ("Evaluating RLE+Deflate with Zopfli: " <> comparison)
      return result

    let (bestFilters, bestStream) =
          minimumBy eMinOrder (SQ.fromList [zopfli, rle, rleZopfli])
    return
      $   PDFIndirectObject
            num
            gen
            (PDFDictionary $ HM.insert "Filter" (reduceArray bestFilters) dict)
      .   Just
      <$> bestStream

filterOptimize object = return (pure object)

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
refilter :: StepM m => PDFObject -> m (Either UnifiedError PDFObject)
refilter object@(PDFIndirectObject _ _ (PDFDictionary _) (Just _)) = do
  step ("Packing strings of object " <> objectNumberText object)
  let oStream = unfilter (deepMap optimizeString object) >>= streamOptimize
  case oStream of
    Right oObject -> filterOptimize oObject
    Left  _       -> return oStream

refilter object = do
  step ("Packing strings of object " <> objectNumberText object)
  return (pure $ deepMap optimizeString object)

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

objectNumberText :: PDFObject -> T.Text
objectNumberText (PDFIndirectObject number version _ _) =
  T.pack (show number) <> "." <> T.pack (show version)
objectNumberText _ = "anonymous"

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
optimize :: StepM m => PDFObject -> m PDFObject
optimize object
  | optimizable object = do
    step ("Optimizing object " <> objectNumberText object)
    refiltered <- refilter object
    return $ case refiltered of
      Right optimizedObject -> optimizedObject
      Left  _               -> object
  | otherwise = do
    action ("Ignoring object " <> objectNumberText object) object
