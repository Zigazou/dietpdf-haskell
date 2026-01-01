{-|
This module contains functions facilitating container manipulation (`PDFArray`,
`PDFDictionary` and `PDFIndirectObject`).
-}
module PDF.Object.Container
  ( deepMap
  , setFilters
  , getFilters
  ) where

import Data.Functor ((<&>))
import Data.Logging (Logging)
import Data.Map.Strict qualified as Map
import Data.PDF.Filter (Filter (Filter))
import Data.PDF.FilterList (FilterList, filtersFilter, filtersParms)
import Data.PDF.PDFObject
    ( PDFObject (PDFArray, PDFDictionary, PDFIndirectObject, PDFIndirectObjectWithStream, PDFName, PDFNull, PDFObjectStream, PDFReference)
    , hasDictionary
    )
import Data.PDF.PDFWork (PDFWork, throwError, getReference)
import Data.Sequence qualified as SQ
import Data.UnifiedError (UnifiedError (InvalidFilterParm))

import PDF.Object.State (embedObject, getValue, updateValue)

{-|
Apply a function to any object contained by an object at any level.
-}
deepMap
  :: Logging m
  => (PDFObject -> PDFWork m PDFObject)
  -> PDFObject
  -> PDFWork m PDFObject
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

{-|
Return a list of filters contained in a `PDFDictionary`.
-}
getFilters :: Logging m => PDFObject -> PDFWork m FilterList
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
    (Just reference@(PDFReference  _ _), Just p) -> do
      f <- getReference reference
      return $ group (SQ.singleton f) (SQ.singleton p)
    (Just reference@(PDFReference  _ _), Nothing) -> do
      f <- getReference reference
      return $ group (SQ.singleton f) SQ.empty

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
Update the Filter and DecodeParms dictionary entries according to a
`FilterList`.

This function works on any `PDFObject` having a dictionary, it does not check
that the object has a stream.

It does nothing on any other object.
-}
setFilters :: Logging m => FilterList -> PDFObject -> PDFWork m PDFObject
setFilters filters object = if hasDictionary object
  then updateValue "Filter" (filtersFilter filters) object
          >>= updateValue "DecodeParms" (filtersParms filters)
  else return object
