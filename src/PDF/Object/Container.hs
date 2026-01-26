{-|
Container manipulation utilities for PDF objects

This module provides utility functions for manipulating PDF container objects:

- 'PDFArray': Sequences of PDF objects
- 'PDFDictionary': Key-value mappings of PDF objects
- 'PDFIndirectObject': Objects referenced by number and generation

Key functionality includes deep mapping over nested structures, and filter
manipulation for stream compression and decompression parameters.
-}
module PDF.Object.Container
  ( setFilters
  , getFilters
  ) where

import Data.Logging (Logging)
import Data.PDF.Filter (Filter (Filter))
import Data.PDF.FilterList (FilterList, filtersFilter, filtersParms)
import Data.PDF.PDFObject
  (PDFObject (PDFArray, PDFName, PDFNull, PDFReference), hasDictionary)
import Data.PDF.PDFWork (PDFWork, getReference, throwError)
import Data.Sequence qualified as SQ
import Data.UnifiedError (UnifiedError (InvalidFilterParm))

import PDF.Object.State (getValue, updateValue)

{-|
Extract and pair compression filters with their parameters from a PDF object.

Retrieves the 'Filter' and 'DecodeParms' entries from a PDF dictionary and zips
them into a 'FilterList' of 'Filter' objects. Each 'Filter' consists of a filter
name and its optional parameters.

Handles multiple input formats:

- Single filter as a name: @/Filter /FlateDecode@
- Single filter as a reference: @/Filter 42 0 R@
- Filter array with 'DecodeParms' as null, missing, single object, or array
- Filter array with corresponding 'DecodeParms' array (zipped together)

If 'DecodeParms' is shorter than the filter list, missing parameters are padded
with 'PDFNull' values.

__Parameters:__

- A PDF object (typically a dictionary-containing object with stream data)

__Returns:__ A 'FilterList' sequence of 'Filter' objects, or empty sequence if
no filters are found.

__Fails:__ If filter format is invalid (neither name nor reference, or
incompatible parameter format).

__Monadic context:__ Runs in the 'PDFWork' monad with 'Logging' capability.
-}
getFilters :: Logging m => PDFObject -> PDFWork m FilterList
getFilters container = do
  filters <- getValue "Filter" container
  parms   <- getValue "DecodeParms" container

  case (filters, parms) of
    -- Array of filters but no parameters.
    (Just (PDFArray fs), Just PDFNull      ) -> return $ group fs SQ.empty
    (Just (PDFArray fs), Nothing           ) -> return $ group fs SQ.empty

    -- Array of filters with an array of parameters.
    (Just (PDFArray fs), Just (PDFArray ps)) -> return $ group fs ps

    -- Array of filters with parameters as a reference.
    (Just (PDFArray fs), Just reference@(PDFReference _ _)) -> do
      p <- getReference reference
      return $ group fs (SQ.singleton p)

    -- Array of filters with a single parameter object.
    (Just (PDFArray fs), Just object) ->
      return $ group fs (SQ.singleton object)

    -- One filter with no parameters.
    (Just f@(PDFName _), Just PDFNull) ->
      return $ group (SQ.singleton f) SQ.empty
    (Just f@(PDFName _), Nothing) -> return $ group (SQ.singleton f) SQ.empty

    -- One filter with an array of parameters.
    (Just f@(PDFName _), Just (PDFArray ps)) ->
      return $ group (SQ.singleton f) ps

    -- Array of filters with parameters as a reference.
    (Just f@(PDFName _), Just reference@(PDFReference _ _)) -> do
      p <- getReference reference
      return $ group (SQ.singleton f) (SQ.singleton p)

    -- One filter with one parameter object.
    (Just f@(PDFName _), Just p) ->
      return $ group (SQ.singleton f) (SQ.singleton p)

    -- One filter as a reference with no parameters.
    (Just reference@(PDFReference  _ _), Just p) -> do
      f <- getReference reference
      return $ group (SQ.singleton f) (SQ.singleton p)
    (Just reference@(PDFReference  _ _), Nothing) -> do
      f <- getReference reference
      return $ group (SQ.singleton f) SQ.empty

    -- No filters.
    (Nothing, _) -> return SQ.empty
    (_      , _) -> throwError (InvalidFilterParm (show filters
                                                ++ " "
                                                ++ show parms
                                                )
                               )
 where
  group :: SQ.Seq PDFObject -> SQ.Seq PDFObject -> FilterList
  group fs ps = SQ.zipWith
    Filter
    fs
    (ps SQ.>< SQ.replicate (SQ.length fs - SQ.length ps) PDFNull)

{-|
Set or update the Filter and DecodeParms dictionary entries in a PDF object.

Updates a PDF object's dictionary with new filter and decoding parameter entries
based on a 'FilterList'. This is typically used after modifying the stream
compression or decompression chain.

Only operates on objects that have dictionaries (verified via 'hasDictionary').
Does nothing if the object is not a dictionary-containing type.

Updates both entries:

- 'Filter': Extracted from the filter list (single name or array of names)
- 'DecodeParms': Extracted from the filter list (single dict or array of dicts)

__Parameters:__

- A 'FilterList' containing the new filters and parameters
- A PDF object to update

__Returns:__ The updated object with new Filter and DecodeParms entries, or the
original object unchanged if it has no dictionary.

__Monadic context:__ Runs in the 'PDFWork' monad with 'Logging' capability.
-}
setFilters :: Logging m => FilterList -> PDFObject -> PDFWork m PDFObject
setFilters filters object = if hasDictionary object
  then updateValue "Filter" (filtersFilter filters) object
          >>= updateValue "DecodeParms" (filtersParms filters)
  else return object
