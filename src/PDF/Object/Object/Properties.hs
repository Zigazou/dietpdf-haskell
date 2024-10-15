module PDF.Object.Object.Properties
  ( hasKey
  , getValueForKey
  , updateStream
  , xrefCount
  , objectType
  , isInfo
  , isCatalog
  ) where

import Data.ByteString qualified as BS
import Data.List (foldl')
import Data.Map.Strict qualified as Map
import Data.PDF.PDFObject
    ( PDFObject (PDFDictionary, PDFIndirectObject, PDFIndirectObjectWithStream, PDFName, PDFObjectStream, PDFTrailer, PDFXRef, PDFXRefStream)
    )
import Data.PDF.XRefSubsection (xrssCount)

import PDF.Object.Object.ToPDFNumber (mkPDFNumber)

import Util.Dictionary (Dictionary, dictHasKey)

{- |
Update the stream embedded in a `PDFObject`.

It also updates the Length entry in the associated dictionary to reflect the
change.
-}
updateStream :: PDFObject -> BS.ByteString -> PDFObject
updateStream object newStream = case object of
  (PDFIndirectObjectWithStream number revision dict _) ->
    PDFIndirectObjectWithStream number revision (newDict dict) newStream
  (PDFObjectStream number revision dict _) ->
    PDFObjectStream number revision (newDict dict) newStream
  (PDFXRefStream number revision dict _) ->
    PDFXRefStream number revision (newDict dict) newStream
  _anyOtherObject -> object
 where
  newLength :: PDFObject
  newLength = mkPDFNumber (BS.length newStream)

  newDict :: Dictionary PDFObject -> Dictionary PDFObject
  newDict = Map.adjust (const newLength) "Length"

{- |
Returns the count of cross-references in a `PDFXRef` object.

If the object is not a `PDFXRef`, it returns 0.
-}
xrefCount :: PDFObject -> Int
xrefCount (PDFXRef subsections) = foldl' (+) 0 $ xrssCount <$> subsections
xrefCount (PDFXRefStream _ _ _ stream) = BS.length stream
xrefCount _ = 0

{- |
Given a `Dictionary`, generate a human `ByteString` describing the object based
on the Type and Subtype keys.

If the dictionary contains does not contain a Type key, it returns `Nothing`.
-}
objectType :: Dictionary PDFObject -> Maybe BS.ByteString
objectType dictionary =
  case (Map.lookup "Type" dictionary, Map.lookup "SubType" dictionary) of
    (Just (PDFName typeValue), Just (PDFName subtypeValue)) ->
      Just $ BS.concat [typeValue, "/", subtypeValue]
    (Just (PDFName typeValue), _noSubtype) -> Just typeValue
    _noType -> Nothing

{- |
Determine if a key is in a dictionary from a `PDFObject`.

If the `PDFObject` has no dictionary, it returns `False`.
-}
hasKey
  :: BS.ByteString -- ^ The key to search for
  -> PDFObject -- ^ The `PDFObject` to search in
  -> Bool
hasKey key (PDFDictionary dict                        ) = dictHasKey key dict
hasKey key (PDFIndirectObjectWithStream _ _ dict _    ) = dictHasKey key dict
hasKey key (PDFObjectStream             _ _ dict _    ) = dictHasKey key dict
hasKey key (PDFXRefStream               _ _ dict _    ) = dictHasKey key dict
hasKey key (PDFIndirectObject _ _ (PDFDictionary dict)) = dictHasKey key dict
hasKey key (PDFTrailer (PDFDictionary dict)           ) = dictHasKey key dict
hasKey _   _anyOtherObject                              = False

getValueForKey :: BS.ByteString -> PDFObject -> Maybe PDFObject
getValueForKey key (PDFDictionary dict) = Map.lookup key dict
getValueForKey key (PDFIndirectObject _ _ (PDFDictionary dict)) = Map.lookup key dict
getValueForKey key (PDFIndirectObjectWithStream _ _ dict _) = Map.lookup key dict
getValueForKey key (PDFObjectStream _ _ dict _) = Map.lookup key dict
getValueForKey key (PDFXRefStream _ _ dict _) = Map.lookup key dict
getValueForKey key (PDFTrailer (PDFDictionary dict)) = Map.lookup key dict
getValueForKey _ _ = Nothing

{- |
Checks if the given PDF object contains document information (e.g., has an
"Author" key).

Returns `True` if the object contains document info, `False` otherwise.
-}
isInfo :: PDFObject -> Bool
isInfo object@PDFIndirectObject{} = hasKey "Author" object
isInfo _anyOtherObject            = False

{- |
Checks if the given PDF object is a Catalog object by verifying that its "Type"
key is set to "Catalog".

Returns `True` if it is a Catalog, `False` otherwise.
-}
isCatalog :: PDFObject -> Bool
isCatalog (PDFIndirectObject _ _ (PDFDictionary dict)) =
  case Map.lookup "Type" dict of
    Just (PDFName "Catalog") -> True
    _anyOtherValue           -> False
isCatalog _anyOtherObject = False
