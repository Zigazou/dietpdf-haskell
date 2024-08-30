{- |
This module contains functions facilitating container manipulation (`PDFArray`,
`PDFDictionary` and `PDFIndirectObject`).
-}
module Pdf.Object.Container
  ( deepMap
  , Filter(Filter, fDecodeParms, fFilter)
  , FilterList
  , mkFilterList
  , setFilters
  , filtersFilter
  , filtersParms
  , getFilters
  ) where

import Control.Monad.Trans.Except (throwE)

import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as SQ

import Pdf.Object.Object
    ( PDFObject (PDFArray, PDFDictionary, PDFIndirectObject, PDFIndirectObjectWithStream, PDFName, PDFNull, PDFObjectStream)
    , hasDictionary
    )
import Pdf.Object.State (embedObject, getValue, updateValue)

import Util.Logging (Logging)
import Util.UnifiedError (FallibleT, UnifiedError (InvalidFilterParm))

{- |
Apply a function to any object contained by an object at any level.
-}
deepMap
  :: Logging m
  => (PDFObject -> FallibleT m PDFObject)
  -> PDFObject
  -> FallibleT m PDFObject
deepMap fn container = case container of
  PDFIndirectObject _ _ object ->
    deepMap fn object >>= flip embedObject container
  PDFIndirectObjectWithStream _ _ dict _ ->
    deepMap fn (PDFDictionary dict) >>= flip embedObject container
  PDFObjectStream _ _ dict _ ->
    deepMap fn (PDFDictionary dict) >>= flip embedObject container
  PDFDictionary dict ->
    sequence (Map.map (deepMap fn) dict)
      >>= flip embedObject container
      .   PDFDictionary
  PDFArray items -> mapM (deepMap fn) items <&> PDFArray
  object         -> fn object

{- |
A filter with its parameters.
-}
type Filter :: Type
data Filter = Filter
  { fFilter      :: !PDFObject
  , fDecodeParms :: !PDFObject
  }
  deriving stock Show

{-# INLINE hasNoDecodeParms #-}
hasNoDecodeParms :: Filter -> Bool
hasNoDecodeParms = (== PDFNull) . fDecodeParms

-- | A list of `Filter`.
type FilterList :: Type
type FilterList = SQ.Seq Filter

-- | Create a `FilterList`.
{-# INLINE mkFilterList #-}
mkFilterList :: [Filter] -> FilterList
mkFilterList = SQ.fromList

{- |
Return a list of filters contained in a `PDFDictionary`.
-}
getFilters :: Logging m => PDFObject -> FallibleT m FilterList
getFilters container = do
  filters <- getValue "Filter" container
  parms   <- getValue "DecodeParms" container

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
    (_      , _) -> throwE InvalidFilterParm
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
setFilters :: Logging m => FilterList -> PDFObject -> FallibleT m PDFObject
setFilters filters object = if hasDictionary object
  then updateValue "Filter" (filtersFilter filters) object
          >>= updateValue "DecodeParms" (filtersParms filters)
  else return object
