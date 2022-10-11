{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

{- |
This module contains functions facilitating container manipulation (`PDFArray`,
`PDFDictionary` and `PDFIndirectObject`).
-}
module Pdf.Object.Container
  ( deepMap
  , deepMapM
  , Filter(Filter, fDecodeParms, fFilter)
  , FilterList
  , mkFilterList
  , setFilters
  , filtersFilter
  , filtersParms
  , insertMaybe
  , insertMaybes
  , getFilters
  ) where

import           Control.Monad.State            ( get
                                                , lift
                                                , put
                                                )
import qualified Data.ByteString               as BS
import qualified Data.Map.Strict               as Map
import qualified Data.Sequence                 as SQ
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFArray
                                                  , PDFDictionary
                                                  , PDFIndirectObject
                                                  , PDFIndirectObjectWithStream
                                                  , PDFName
                                                  , PDFNull
                                                  , PDFObjectStream
                                                  )
                                                )
import           Pdf.Object.State               ( (?=)
                                                , FallibleComputation
                                                , ObjectComputation
                                                , embedObject
                                                , getValue
                                                , hasDictionaryS
                                                , ifObject
                                                , modifyObject
                                                , updateE
                                                )
import           Util.Dictionary                ( Dictionary )
import           Util.Errors                    ( UnifiedError
                                                  ( InvalidFilterParm
                                                  )
                                                , unifiedError
                                                )

{- |
Apply a function to any object contained by an object at any level.
-}
deepMap
  :: (PDFObject -> Either UnifiedError PDFObject) -> FallibleComputation ()
deepMap fn = get >>= \case
  PDFIndirectObject{}           -> modifyObject (`updateE` deepMap fn)
  PDFIndirectObjectWithStream{} -> modifyObject (`updateE` deepMap fn)
  PDFObjectStream{}             -> modifyObject (`updateE` deepMap fn)
  PDFDictionary dict ->
    lift (sequence (Map.map (`updateE` deepMap fn) dict))
      >>= embedObject
      .   PDFDictionary
  PDFArray items ->
    lift (sequence (flip updateE (deepMap fn) <$> items)) >>= put . PDFArray
  object -> lift (fn object) >>= put

{- |
Apply a function to any object contained by an object at any level.
-}
deepMapM :: FallibleComputation () -> FallibleComputation ()
deepMapM fn = get >>= \case
  PDFIndirectObject _ _ object ->
    lift (updateE object (deepMapM fn)) >>= embedObject
  PDFIndirectObjectWithStream _ _ dict _ -> do
    lift (updateE (PDFDictionary dict) (deepMapM fn)) >>= embedObject
  PDFObjectStream _ _ dict _ ->
    lift (updateE (PDFDictionary dict) (deepMapM fn)) >>= embedObject
  PDFDictionary dict ->
    lift (sequence (Map.map (`updateE` deepMapM fn) dict))
      >>= embedObject
      .   PDFDictionary
  PDFArray items ->
    lift (sequence (flip updateE (deepMapM fn) <$> items)) >>= put . PDFArray
  object -> lift (updateE object fn) >>= put

{- |
A filter with its parameters.
-}
data Filter = Filter
  { fFilter      :: PDFObject
  , fDecodeParms :: PDFObject
  }
  deriving stock Show

hasNoDecodeParms :: Filter -> Bool
hasNoDecodeParms = (== PDFNull) . fDecodeParms

-- | A list of `Filter`.
type FilterList = SQ.Seq Filter

-- | Create a `FilterList`.
mkFilterList :: [Filter] -> FilterList
mkFilterList = SQ.fromList

{- |
Return a list of filters contained in a `PDFDictionary`.
-}
getFilters :: FallibleComputation FilterList
getFilters = do
  filters <- getValue "Filter"
  parms   <- getValue "DecodeParms"

  case (filters, parms) of
    (Just (PDFArray fs), Just PDFNull      ) -> return $ group fs SQ.empty
    (Just (PDFArray fs), Nothing           ) -> return $ group fs SQ.empty
    (Just (PDFArray fs), Just (PDFArray ps)) -> return $ group fs ps
    (Just (PDFArray fs), Just object) ->
      return $ group fs (SQ.singleton object)

    (Just f@(PDFName _), Just PDFNull) ->
      return $ group (SQ.singleton f) SQ.empty
    (Just f@(PDFName _), Nothing) -> return $ group (SQ.singleton f) SQ.empty
    (Just f@(PDFName _), Just (PDFArray ps)) ->
      return $ group (SQ.singleton f) ps
    (Just f@(PDFName _), Just p) ->
      return $ group (SQ.singleton f) (SQ.singleton p)

    (Nothing, _) -> return SQ.empty
    (_      , _) -> unifiedError InvalidFilterParm
 where
  group :: SQ.Seq PDFObject -> SQ.Seq PDFObject -> FilterList
  group fs ps = SQ.zipWith
    Filter
    fs
    (ps SQ.>< SQ.replicate (SQ.length fs - SQ.length ps) PDFNull)

{- |
Given a list of `Filter`, return the corresponding `PDFObject` of filter names.

If the list is empty, it returns `Nothing`.

If the list contains only one `Filter`, it returns `Just` a `PDFName`.

In any other cases, it returns `Just` a `PDFArray`.
-}
filtersFilter :: FilterList -> Maybe PDFObject
filtersFilter SQ.Empty = Nothing
filtersFilter (Filter aName@(PDFName _) _ SQ.:<| SQ.Empty) = Just aName
filtersFilter filters  = Just (PDFArray $ fFilter <$> filters)

{- |
Given a list of `Filter`, return the corresponding `PDFObject` of filters
decoding parameters.

If the list is empty, it returns `Nothing`.

If the list contains only one `Filter`, it returns `Just` a `PDFObject`.

In any other cases, it returns `Just` a `PDFArray`.
-}
filtersParms :: FilterList -> Maybe PDFObject
filtersParms SQ.Empty = Nothing
filtersParms (Filter _ PDFNull SQ.:<| SQ.Empty) = Nothing
filtersParms (Filter _ aDecodeParms SQ.:<| SQ.Empty) = Just aDecodeParms
filtersParms filters | all hasNoDecodeParms filters = Nothing
                     | otherwise = Just (PDFArray $ fDecodeParms <$> filters)

{- |
Update the Filter and DecodeParms dictionary entries according to a
`FilterList`.

This function works on any `PDFObject` having a dictionary, it does not check
that the object has a stream.

It does nothing on any other object.
-}
setFilters :: Monad m => FilterList -> ObjectComputation m ()
setFilters filters = ifObject hasDictionaryS $ do
  "Filter" ?= filtersFilter filters
  "DecodeParms" ?= filtersParms filters

{- |
Insert a key-value pair inside a `Dictionary`.

If the value is `Nothing`, the dictionary is returned without modification.

It the value is `Just` something, the something is inserted into the dictionary.
-}
insertMaybe
  :: Dictionary PDFObject -- ^ The dictionary to update
  -> BS.ByteString -- ^ The key name
  -> Maybe PDFObject -- ^ The `Maybe` value to associate to the key name
  -> Dictionary PDFObject -- ^ The resulting dictionary
insertMaybe dict name (Just object) = Map.insert name object dict
insertMaybe dict _    Nothing       = dict

{- |
Insert a list of key-value pair inside a `Dictionary`.

Any `Nothing` value is ignored.
-}
insertMaybes
  :: Dictionary PDFObject -- ^ The original `Dictionary`
  -> [(BS.ByteString, Maybe PDFObject)] -- ^ Key-values to insert
  -> Dictionary PDFObject -- ^ The updated `Dictionary`
insertMaybes dict [] = dict
insertMaybes dict ((name, value) : remains) =
  insertMaybes (insertMaybe dict name value) remains
