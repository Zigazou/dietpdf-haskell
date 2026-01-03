{-|
Property queries and modifications for PDF objects

This module provides utility functions for querying and modifying properties of
PDF objects, including dictionary access, stream manipulation, and object type
identification.
-}
module PDF.Object.Object.Properties
  ( hasKey
  , getValueForKey
  , setValueForKey
  , updateStream
  , xrefCount
  , objectType
  , isInfo
  , isCatalog
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.List (foldl')
import Data.Map.Strict qualified as Map
import Data.PDF.PDFObject
    ( PDFObject (PDFDictionary, PDFIndirectObject, PDFIndirectObjectWithStream, PDFName, PDFObjectStream, PDFTrailer, PDFXRef, PDFXRefStream)
    )
import Data.PDF.XRefSubsection (xrssCount)

import PDF.Object.Object.ToPDFNumber (mkPDFNumber)

import Util.Dictionary (Dictionary, dictAlter, dictHasKey)

{-|
Replace the stream content in a PDF object and update the Length entry.

This function updates the stream data in indirect objects that contain streams
(such as @PDFIndirectObjectWithStream@, @PDFObjectStream@, or @PDFXRefStream@).
It also automatically updates the @Length@ entry in the associated dictionary to
reflect the new stream size.

For objects that do not contain streams, the function returns the object
unchanged.

__Parameters:__

- The PDF object to update
- The new stream content as a bytestring

__Returns:__ The modified PDF object with updated stream and Length dictionary
entry.
-}
updateStream :: PDFObject -> ByteString -> PDFObject
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

{-|
Calculate the total number of cross-reference entries in a PDF object.

For @PDFXRef@ objects, this sums the entry counts from all subsections. For
@PDFXRefStream@ objects, this returns the length of the stream content. For all
other object types, this returns 0.

__Parameters:__

- The PDF object to examine

__Returns:__ The total count of cross-reference entries, or 0 if the object is
not an xref structure.
-}
xrefCount :: PDFObject -> Int
xrefCount (PDFXRef subsections) = foldl' (+) 0 $ xrssCount <$> subsections
xrefCount (PDFXRefStream _ _ _ stream) = BS.length stream
xrefCount _ = 0

{-|
Determine the type of a PDF object based on its dictionary.

This function examines the @Type@ and @SubType@ entries in a dictionary to
generate a human-readable type description. If both @Type@ and @SubType@ are
present as names, the result is @Type/SubType@. If only @Type@ is present, the
result is just the @Type@ value. If neither is present, returns 'Nothing'.

__Parameters:__

- A dictionary to examine

__Returns:__

- 'Just' with a bytestring describing the object type (e.g., @\"Page\"@,
  @\"Font/Type1\"@)
- 'Nothing' if the dictionary does not contain a @Type@ entry
-}
objectType :: Dictionary PDFObject -> Maybe ByteString
objectType dictionary =
  case (Map.lookup "Type" dictionary, Map.lookup "SubType" dictionary) of
    (Just (PDFName typeValue), Just (PDFName subtypeValue)) ->
      Just $ BS.concat [typeValue, "/", subtypeValue]
    (Just (PDFName typeValue), _noSubtype) -> Just typeValue
    _noType -> Nothing

{-|
Test whether a key exists in a dictionary within a PDF object.

This function examines the dictionary associated with various PDF object types
and checks for the presence of a specific key. Objects without dictionaries
always return 'False'.

__Parameters:__

- The key to search for
- The PDF object to search in

__Returns:__ 'True' if the key exists in the object's dictionary, 'False'
otherwise.
-}
hasKey
  :: ByteString -- ^ The key to search for
  -> PDFObject -- ^ The `PDFObject` to search in
  -> Bool
hasKey key (PDFDictionary dict                        ) = dictHasKey key dict
hasKey key (PDFIndirectObjectWithStream _ _ dict _    ) = dictHasKey key dict
hasKey key (PDFObjectStream             _ _ dict _    ) = dictHasKey key dict
hasKey key (PDFXRefStream               _ _ dict _    ) = dictHasKey key dict
hasKey key (PDFIndirectObject _ _ (PDFDictionary dict)) = dictHasKey key dict
hasKey key (PDFTrailer (PDFDictionary dict)           ) = dictHasKey key dict
hasKey _   _anyOtherObject                              = False

{-|
Retrieve the value associated with a key in a PDF object's dictionary.

This function searches for a key in the dictionary of various PDF object types
and returns the associated value if found. For objects without dictionaries or
when the key is not present, returns 'Nothing'.

__Parameters:__

- The key to look up
- The PDF object to search in

__Returns:__

- 'Just' with the PDF object value if the key exists
- 'Nothing' if the key is not found or the object has no dictionary
-}
getValueForKey :: ByteString -> PDFObject -> Maybe PDFObject
getValueForKey key (PDFDictionary dict) = Map.lookup key dict
getValueForKey key (PDFIndirectObject _ _ (PDFDictionary dict)) = Map.lookup key dict
getValueForKey key (PDFIndirectObjectWithStream _ _ dict _) = Map.lookup key dict
getValueForKey key (PDFObjectStream _ _ dict _) = Map.lookup key dict
getValueForKey key (PDFXRefStream _ _ dict _) = Map.lookup key dict
getValueForKey key (PDFTrailer (PDFDictionary dict)) = Map.lookup key dict
getValueForKey _ _ = Nothing

{-|
Set or delete a key-value pair in a PDF object's dictionary.

This function modifies or creates a key-value pair in the dictionary of various
PDF object types. If the value is 'Just', the key is inserted or updated; if the
value is 'Nothing', the key is deleted. For objects without dictionaries, the
object is returned unchanged.

__Parameters:__

- The key to modify
- The new value: 'Just' to insert/update, 'Nothing' to delete
- The PDF object to modify

__Returns:__ A new PDF object with the dictionary modified according to the
specified operation.
-}
setValueForKey :: ByteString -> Maybe PDFObject -> PDFObject -> PDFObject
setValueForKey key (Just value) (PDFDictionary dict) =
  PDFDictionary (Map.insert key value dict)
setValueForKey key Nothing (PDFDictionary dict) =
  PDFDictionary (Map.delete key dict)
setValueForKey key value (PDFIndirectObject major minor dict) =
  PDFIndirectObject major minor (setValueForKey key value dict)
setValueForKey key value (PDFIndirectObjectWithStream major minor dict stream) =
  PDFIndirectObjectWithStream major minor (dictAlter key value dict) stream
setValueForKey key value (PDFObjectStream major minor dict stream) =
  PDFObjectStream major minor (dictAlter key value dict) stream
setValueForKey key value (PDFXRefStream major minor dict stream) =
  PDFXRefStream major minor (dictAlter key value dict) stream
setValueForKey key value (PDFTrailer dict) =
  PDFTrailer (setValueForKey key value dict)
setValueForKey _anyKey _anyValue object = object

{-|
Test whether a PDF object contains document metadata information.

A PDF object is considered an info dictionary if it is an indirect object and
contains an @Author@ key (commonly present in document information
dictionaries).

__Parameters:__

- The PDF object to test

__Returns:__ 'True' if the object is an indirect object containing an @Author@
key, 'False' otherwise.
-}
isInfo :: PDFObject -> Bool
isInfo object@PDFIndirectObject{} = hasKey "Author" object
isInfo _anyOtherObject            = False

{-|
Test whether a PDF object is a Catalog object.

A PDF object is identified as a Catalog if it is an indirect object containing a
dictionary with a @Type@ entry set to the name @Catalog@. The Catalog is the
root object of a PDF document's object tree.

__Parameters:__

- The PDF object to test

__Returns:__ 'True' if the object is a Catalog (has @Type@ set to @Catalog@),
'False' otherwise.
-}
isCatalog :: PDFObject -> Bool
isCatalog (PDFIndirectObject _ _ (PDFDictionary dict)) =
  case Map.lookup "Type" dict of
    Just (PDFName "Catalog") -> True
    _anyOtherValue           -> False
isCatalog _anyOtherObject = False
