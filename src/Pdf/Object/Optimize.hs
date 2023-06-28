{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE FlexibleContexts #-}

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
import           Pdf.Object.Format              ( txtObjectNumberVersion )
import           Util.Errors                    ( UnifiedError
                                                  ( InvalidFilterParm
                                                  )
                                                )
import           Util.Step                      ( StepM
                                                , stepT
                                                , actionT
                                                , StepT
                                                , except
                                                , catchE
                                                )
import qualified Data.Text                     as T
import           Text.Printf                    ( printf )

data Filter = Filter
  { fFilter      :: PDFObject
  , fDecodeParms :: PDFObject
  }

hasNoDecodeParms :: Filter -> Bool
hasNoDecodeParms = (== PDFNull) . fDecodeParms
{--
unfilterStream
  :: StepM m
  => Either UnifiedError ([Filter], BS.ByteString)
  -> m (Either UnifiedError ([Filter], BS.ByteString))
unfilterStream (Right (filters@(pdfFilter : otherFilters), stream))
  | fFilter pdfFilter == PDFName "FlateDecode" = unfilterStream
    ((otherFilters, ) <$> FL.decompress stream)
  | fFilter pdfFilter == PDFName "RunLengthDecode" = unfilterStream
    ((otherFilters, ) <$> RL.decompress stream)
  | fFilter pdfFilter == PDFName "LZWDecode" = unfilterStream
    ((otherFilters, ) <$> LZ.decompress stream)
  | fFilter pdfFilter == PDFName "ASCII85Decode" = unfilterStream
    ((otherFilters, ) <$> A8.decode stream)
  | fFilter pdfFilter == PDFName "ASCIIHexDecode" = unfilterStream
    ((otherFilters, ) <$> AH.decode stream)
  | otherwise = pure $ Right (filters, stream)
unfilterStream (Right (filters, stream)) = pure $ Right (filters, stream)
unfilterStream unfilterError             = return unfilterError
--}

unfilterStream
  :: StepM m => ([Filter], BS.ByteString) -> StepT m ([Filter], BS.ByteString)
unfilterStream (filters@(pdfFilter : otherFilters), stream)
  | fFilter pdfFilter == PDFName "FlateDecode"
  = except (FL.decompress stream) >>= (unfilterStream . (otherFilters, ))
  | fFilter pdfFilter == PDFName "RunLengthDecode"
  = except (RL.decompress stream) >>= (unfilterStream . (otherFilters, ))
  | fFilter pdfFilter == PDFName "LZWDecode"
  = except (LZ.decompress stream) >>= (unfilterStream . (otherFilters, ))
  | fFilter pdfFilter == PDFName "ASCII85Decode"
  = except (A8.decode stream) >>= (unfilterStream . (otherFilters, ))
  | fFilter pdfFilter == PDFName "ASCIIHexDecode"
  = except (AH.decode stream) >>= (unfilterStream . (otherFilters, ))
  | otherwise
  = return (filters, stream)
unfilterStream (filters, stream) = return (filters, stream)

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

unfilter :: StepM m => PDFObject -> StepT m PDFObject
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
  unfiltered :: StepM m => StepT m ([Filter], BS.ByteString)
  unfiltered = except (getFilters dict) >>= unfilterStream . (, stream)
unfilter anyOtherObject = return anyOtherObject

eMinOrder :: (a, BS.ByteString) -> (a, BS.ByteString) -> Ordering
eMinOrder (_, x) (_, y) = compare (BS.length x) (BS.length y)

reduceArray :: [PDFObject] -> PDFObject
reduceArray [item] = item
reduceArray items  = PDFArray items

sizeComparison :: BS.ByteString -> ([PDFObject], BS.ByteString) -> T.Text
sizeComparison before (_, after) = T.pack
  $ printf "%d/%d (%+.2f%%)" sizeBefore sizeAfter ratio
 where
  sizeBefore = BS.length before
  sizeAfter  = BS.length after
  ratio :: Float
  ratio =
    100
      * (fromIntegral sizeAfter - fromIntegral sizeBefore)
      / fromIntegral sizeBefore

shouldTryCombining :: BS.ByteString -> BS.ByteString -> Bool
shouldTryCombining raw rle = BS.length raw > BS.length rle

filterOptimize :: StepM m => PDFObject -> StepT m PDFObject
filterOptimize (PDFIndirectObject num gen (PDFDictionary dict) (Just stream)) =
  do
    zopfli     <- evaluateZopfli stream
    rle        <- evaluateRle stream

    evaluateds <- if shouldTryCombining stream (snd rle)
      then do
        rleZopfli <- evaluateRleZopfli stream (snd rle)
        return [zopfli, rle, rleZopfli]
      else return [zopfli, rle]

    let (bestFilters, bestStream) =
          minimumBy eMinOrder (SQ.fromList evaluateds)

    return $ PDFIndirectObject
      num
      gen
      (PDFDictionary $ HM.insert "Filter" (reduceArray bestFilters) dict)
      (Just bestStream)
 where
  evaluateZopfli
    :: StepM m => BS.ByteString -> StepT m ([PDFObject], BS.ByteString)
  evaluateZopfli before = do
    compressedStream <- except (FL.compress before)
    let result     = ([PDFName "FlateDecode"], compressedStream)
        comparison = sizeComparison before result
    stepT ("  - Evaluating Zopfli    : " <> comparison)
    return result

  evaluateRle
    :: StepM m => BS.ByteString -> StepT m ([PDFObject], BS.ByteString)
  evaluateRle before = do
    do
      compressedStream <- except (RL.compress before)
      let result     = ([PDFName "RunLengthDecode"], compressedStream)
          comparison = sizeComparison before result
      stepT ("  - Evaluating RLE       : " <> comparison)
      return result

  evaluateRleZopfli
    :: StepM m
    => BS.ByteString
    -> BS.ByteString
    -> StepT m ([PDFObject], BS.ByteString)
  evaluateRleZopfli before rle = do
    compressedStream <- except (FL.compress rle)
    let
      result =
        ([PDFName "FlateDecode", PDFName "RunLengthDecode"], compressedStream)
      comparison = sizeComparison before result
    stepT ("  - Evaluating RLE+Zopfli: " <> comparison)
    return result

filterOptimize object = return object

streamIsXML :: PDFObject -> Bool
streamIsXML (PDFIndirectObject _ _ (PDFDictionary dictionary) (Just _)) =
  case dictionary HM.!? "Subtype" of
    Just (PDFName "XML") -> True
    _anyOtherValue       -> False
streamIsXML _ = False

streamOptimize :: StepM m => PDFObject -> StepT m PDFObject
streamOptimize object@(PDFIndirectObject _ _ (PDFDictionary _) (Just stream))
  | streamIsXML object = do
    stepT "  - Packing XML stream"
    return $ updateStream object (optimizeXML stream)
  | otherwise = return object
streamOptimize object = return object

{- |
Completely refilter a stream by finding the best filter combination.

It also optimized nested strings and XML streams.
-}
refilter :: StepM m => PDFObject -> StepT m PDFObject
refilter object@(PDFIndirectObject _ _ (PDFDictionary _) (Just _)) = do
  stepT "  - Packing strings"
  {--let oStream = unfilter (deepMap optimizeString object) >>= streamOptimize
  case oStream of
    Right oObject -> filterOptimize oObject
    Left  _       -> return oStream--}
  unfilter (deepMap optimizeString object) >>= streamOptimize >>= filterOptimize

refilter object = do
  stepT "  - Packing strings"
  return (deepMap optimizeString object)

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
optimize :: (StepM m) => PDFObject -> StepT m PDFObject
optimize object
  | optimizable object = do
    stepT (txtObjectNumberVersion object)
    catchE
      (refilter object)
      (\unifiedError -> do
        stepT
          ("  - Unable to optimize (" <> (T.pack . show) unifiedError <> ")")
        return object
      )
  | otherwise = do
    actionT (txtObjectNumberVersion object <> ": ignored") object
