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
  , setFilters
  , filtersFilter
  , filtersParms
  , insertMaybe
  , insertMaybes
  , getFilters
  ) where

import           Control.Monad.State            ( StateT
                                                , get
                                                , lift
                                                , put
                                                )
import qualified Data.ByteString               as BS
import qualified Data.HashMap.Strict           as HM
import           Pdf.Object.Object              ( (?=)
                                                , Dictionary
                                                , PDFObject
                                                  ( PDFArray
                                                  , PDFDictionary
                                                  , PDFIndirectObject
                                                  , PDFIndirectObjectWithStream
                                                  , PDFName
                                                  , PDFNull
                                                  , PDFObjectStream
                                                  )
                                                , getValue
                                                , modifyObject
                                                , embedObject
                                                , updateE
                                                )
import           Util.Errors                    ( UnifiedError
                                                  ( InvalidFilterParm
                                                  )
                                                , unifiedError
                                                )

{- |
Apply a function to any object contained by an object at any level.
-}
deepMap
  :: (PDFObject -> Either UnifiedError PDFObject)
  -> StateT PDFObject (Either UnifiedError) ()
deepMap fn = get >>= \case
  PDFIndirectObject{}           -> modifyObject (`updateE` deepMap fn)
  PDFIndirectObjectWithStream{} -> modifyObject (`updateE` deepMap fn)
  PDFObjectStream{}             -> modifyObject (`updateE` deepMap fn)
  PDFDictionary dict ->
    lift (sequence (HM.map (`updateE` deepMap fn) dict))
      >>= embedObject
      .   PDFDictionary
  PDFArray items ->
    lift (sequence (flip updateE (deepMap fn) <$> items)) >>= put . PDFArray
  object -> lift (fn object) >>= put

{- |
Apply a function to any object contained by an object at any level.
-}
deepMapM
  :: StateT PDFObject (Either UnifiedError) ()
  -> StateT PDFObject (Either UnifiedError) ()
deepMapM fn = get >>= \case
  PDFIndirectObject _ _ object ->
    lift (updateE object (deepMapM fn)) >>= embedObject
  PDFIndirectObjectWithStream _ _ dict _ -> do
    lift (updateE (PDFDictionary dict) (deepMapM fn)) >>= embedObject
  PDFObjectStream _ _ dict _ ->
    lift (updateE (PDFDictionary dict) (deepMapM fn)) >>= embedObject
  PDFDictionary dict ->
    lift (sequence (HM.map (`updateE` deepMapM fn) dict))
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

{- |
Return a list of filters contained in a `PDFDictionary`.
-}
getFilters :: StateT PDFObject (Either UnifiedError) [Filter]
getFilters = do
  filters <- getValue "Filter"
  parms   <- getValue "DecodeParms"

  case (filters, parms) of
    (Just (  PDFArray fs), Just PDFNull      ) -> return $ group fs []
    (Just (  PDFArray fs), Nothing           ) -> return $ group fs []
    (Just (  PDFArray fs), Just (PDFArray ps)) -> return $ group fs ps
    (Just (  PDFArray fs), Just object       ) -> return $ group fs [object]

    (Just f@(PDFName  _ ), Just PDFNull      ) -> return $ group [f] []
    (Just f@(PDFName  _ ), Nothing           ) -> return $ group [f] []
    (Just f@(PDFName  _ ), Just (PDFArray ps)) -> return $ group [f] ps
    (Just f@(PDFName  _ ), Just p            ) -> return $ group [f] [p]

    (Nothing             , _                 ) -> return []
    (_                   , _                 ) -> unifiedError InvalidFilterParm
 where
  group :: [PDFObject] -> [PDFObject] -> [Filter]
  group fs ps = zipWith Filter fs (ps ++ repeat PDFNull)

{- |
Given a list of `Filter`, return the corresponding `PDFObject` of filter names.

If the list is empty, it returns `Nothing`.

If the list contains only one `Filter`, it returns `Just` a `PDFName`.

In any other cases, it returns `Just` a `PDFArray`.
-}
filtersFilter :: [Filter] -> Maybe PDFObject
filtersFilter [] = Nothing
filtersFilter [Filter aName@(PDFName _) _] = Just aName
filtersFilter filters = Just (PDFArray $ fFilter <$> filters)

{- |
Given a list of `Filter`, return the corresponding `PDFObject` of filters
decoding parameters.

If the list is empty, it returns `Nothing`.

If the list contains only one `Filter`, it returns `Just` a `PDFObject`.

In any other cases, it returns `Just` a `PDFArray`.
-}
filtersParms :: [Filter] -> Maybe PDFObject
filtersParms []                      = Nothing
filtersParms [Filter _ PDFNull     ] = Nothing
filtersParms [Filter _ aDecodeParms] = Just aDecodeParms
filtersParms filters | all hasNoDecodeParms filters = Nothing
                     | otherwise = Just (PDFArray $ fDecodeParms <$> filters)

setFilters :: Monad m => [Filter] -> StateT PDFObject m ()
setFilters filters = get >>= \case
  PDFIndirectObject{} -> do
    "Filter" ?= filtersFilter filters
    "DecodeParms" ?= filtersParms filters
  PDFIndirectObjectWithStream{} -> do
    "Filter" ?= filtersFilter filters
    "DecodeParms" ?= filtersParms filters
  PDFObjectStream{} -> do
    "Filter" ?= filtersFilter filters
    "DecodeParms" ?= filtersParms filters
  _anyOtherValue -> return ()

{- |
Insert a key-value pair inside a `Dictionary`.

If the value is `Nothing`, the dictionary is returned without modification.

It the value is `Just` something, the something is inserted into the dictionary.
-}
insertMaybe
  :: Dictionary -- ^ The dictionary to update
  -> BS.ByteString -- ^ The key name
  -> Maybe PDFObject -- ^ The `Maybe` value to associate to the key name
  -> Dictionary -- ^ The resulting dictionary
insertMaybe dict name (Just object) = HM.insert name object dict
insertMaybe dict _    Nothing       = dict

{- |
Insert a list of key-value pair inside a `Dictionary`.

Any `Nothing` value is ignored.
-}
insertMaybes :: Dictionary -> [(BS.ByteString, Maybe PDFObject)] -> Dictionary
insertMaybes dict [] = dict
insertMaybes dict ((name, value) : remains) =
  insertMaybes (insertMaybe dict name value) remains
