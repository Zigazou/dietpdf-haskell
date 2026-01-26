{-|
Utilities for working with PDF filter chains.

In PDF, a stream can declare one or more filters (in the @/Filter@ entry) and
optionally per-filter decode parameters (in the @/DecodeParms@ entry).

This module provides the `FilterList` type and helpers to build the
corresponding `PDFObject` values.
-}
module Data.PDF.FilterList
  ( FilterList
  , mkFilterList
  , filtersFilter
  , filtersParms
  ) where

import Data.Kind (Type)
import Data.PDF.Filter
  (Filter (Filter, fDecodeParms, fFilter), hasNoDecodeParms)
import Data.PDF.PDFObject (PDFObject (PDFArray, PDFName, PDFNull))
import Data.Sequence qualified as SQ

{-|
A list of `Filter` values representing a filter chain.
-}
type FilterList :: Type
type FilterList = SQ.Seq Filter

{-|
Creates a `FilterList` from a regular list.
-}
mkFilterList :: [Filter] -> FilterList
mkFilterList = SQ.fromList

{-|
Given a list of `Filter`, return the corresponding `PDFObject` of filter names.

If the list is empty, it returns `Nothing`.

If the list contains only one `Filter`, it returns `Just` a `PDFName`.

In any other cases, it returns `Just` a `PDFArray`.
-}
filtersFilter :: FilterList -> Maybe PDFObject
filtersFilter SQ.Empty = Nothing
filtersFilter (Filter aName@(PDFName _) _ SQ.:<| SQ.Empty) = Just aName
filtersFilter filters  = Just (PDFArray $ fFilter <$> filters)

{-|
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
