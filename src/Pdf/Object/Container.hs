{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

{- |
This module contains functions facilitating container manipulation (`PDFArray`,
`PDFDictionary` and `PDFIndirectObject`).
-}
module Pdf.Object.Container
  ( deepMap
  , Filter(Filter, fFilter, fDecodeParms)
  , setFilters
  , setDecodeParms
  , insertMaybe
  , insertMaybes
  , getFilters
  ) where

import qualified Data.HashMap.Strict           as HM
import qualified Data.ByteString               as BS
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFIndirectObject
                                                  , PDFIndirectObjectWithStream
                                                  , PDFObjectStream
                                                  , PDFDictionary
                                                  , PDFArray
                                                  , PDFNull
                                                  , PDFName
                                                  )
                                                , Dictionary
                                                )
import           Util.Errors                    ( UnifiedError
                                                  ( InvalidFilterParm
                                                  )
                                                )

{- |
Apply a function to any object contained by an object at any level.
-}
deepMap :: (PDFObject -> PDFObject) -> PDFObject -> PDFObject
deepMap fn (PDFIndirectObject number revision object) =
  PDFIndirectObject number revision (deepMap fn object)
deepMap fn (PDFIndirectObjectWithStream number revision dict stream) =
  PDFIndirectObjectWithStream number revision (deepMapDict fn dict) stream
deepMap fn (PDFObjectStream number revision dict stream) =
  PDFObjectStream number revision (deepMapDict fn dict) stream
deepMap fn (PDFDictionary dictionary) =
  PDFDictionary (deepMapDict fn dictionary)
deepMap fn (PDFArray items) = PDFArray (deepMap fn <$> items)
deepMap fn object           = fn object

deepMapDict :: (PDFObject -> PDFObject) -> Dictionary -> Dictionary
deepMapDict fn = HM.map (deepMap fn)

{- |
A filter with its parameters.
-}
data Filter = Filter
  { fFilter      :: PDFObject
  , fDecodeParms :: PDFObject
  }

hasNoDecodeParms :: Filter -> Bool
hasNoDecodeParms = (== PDFNull) . fDecodeParms

{- |
Return a list of filters contained in a `PDFDictionary`.
-}
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

{- |
Given a list of `Filter`, return the corresponding `PDFObject` of filter names.

If the list is empty, it returns `Nothing`.

If the list contains only one `Filter`, it returns `Just` a `PDFName`.

In any other cases, it returns `Just` a `PDFArray`.
-}
setFilters :: [Filter] -> Maybe PDFObject
setFilters []                           = Nothing
setFilters [Filter aName@(PDFName _) _] = Just aName
setFilters filters                      = Just (PDFArray $ fFilter <$> filters)

{- |
Given a list of `Filter`, return the corresponding `PDFObject` of filters
decoding parameters.

If the list is empty, it returns `Nothing`.

If the list contains only one `Filter`, it returns `Just` a `PDFObject`.

In any other cases, it returns `Just` a `PDFArray`.
-}
setDecodeParms :: [Filter] -> Maybe PDFObject
setDecodeParms []                      = Nothing
setDecodeParms [Filter _ PDFNull     ] = Nothing
setDecodeParms [Filter _ aDecodeParms] = Just aDecodeParms
setDecodeParms filters | all hasNoDecodeParms filters = Nothing
                       | otherwise = Just (PDFArray $ fDecodeParms <$> filters)

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
