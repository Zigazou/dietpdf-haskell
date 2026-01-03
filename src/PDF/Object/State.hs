{-|
State monad utilities for PDF object manipulation

This module provides state monad utilities for querying and modifying PDF
objects in a monadic context.

PDF objects are immutable by design, but this module uses the State and Except
monads to enable safe functional manipulation of PDF document structures. It
provides operations for:

- Querying values and streams from PDF objects
- Setting and removing dictionary entries
- Managing stream data with automatic length updates
- Embedding objects within other PDF structures
-}
module PDF.Object.State
  ( -- * Query
    getValue
  , getValueDefault
  , getStream
  , getDictionary
  , maybeQuery

    -- * Modify
  , setValue
  , removeValue
  , setMaybe
  , updateValue
  , setStream
  , setStream1
  , embedObject
  ) where

import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State (evalStateT)
import Control.Monad.Trans.Except (runExceptT)

import Data.ByteString qualified as BS
import Data.Logging (Logging)
import Data.Map.Strict qualified as Map
import Data.PDF.PDFObject
    ( PDFObject (PDFArray, PDFComment, PDFDictionary, PDFEndOfFile, PDFIndirectObject, PDFIndirectObjectWithStream, PDFObjectStream, PDFStartXRef, PDFTrailer, PDFVersion, PDFXRef, PDFXRefStream)
    )
import Data.PDF.PDFWork (PDFWork, throwError)
import Data.PDF.WorkData (emptyWorkData)
import Data.UnifiedError
    ( UnifiedError (InvalidObjectToEmbed, NoDictionary, NoStream)
    )

import PDF.Object.Object.ToPDFNumber (ToPDFNumber (mkPDFNumber))

import Util.Dictionary (Dictionary)
import Data.ByteString (ByteString)

{-|
Extract the binary stream data from a PDF object.

Retrieves the stream data from objects that contain streams. Returns the raw
bytestring of stream content, which may be compressed or encoded depending on
the object's filter settings.

Only indirect objects with streams, object streams, and cross-reference streams
contain extractable stream data.

__Parameters:__

- A PDF object (may or may not contain stream data)

__Returns:__ The stream data as a bytestring in the 'PDFWork' monad.

__Fails:__ With 'UnifiedError' 'NoStream' if the object does not contain stream
data.
-}
getStream :: Logging m => PDFObject -> PDFWork m ByteString
getStream object = case object of
  (PDFIndirectObjectWithStream _ _ _ stream) -> return stream
  (PDFObjectStream             _ _ _ stream) -> return stream
  (PDFXRefStream               _ _ _ stream) -> return stream
  _anyOtherObject                            -> throwError (NoStream "")

{-|
Extract the dictionary from a PDF object.

Retrieves the dictionary embedded in objects that contain one. Works on multiple
object types:

- 'PDFIndirectObjectWithStream': Dictionary with stream data
- 'PDFObjectStream': Dictionary with stream data
- 'PDFDictionary': Direct dictionary object
- 'PDFIndirectObject': When embedding a dictionary
- 'PDFTrailer': Trailer dictionary
- 'PDFXRefStream': Cross-reference stream dictionary

__Parameters:__

- A PDF object (may or may not contain a dictionary)

__Returns:__ The dictionary in the 'PDFWork' monad.

__Fails:__ With 'UnifiedError' 'NoDictionary' if the object has no dictionary.
-}
getDictionary :: Logging m => PDFObject -> PDFWork m (Dictionary PDFObject)
getDictionary object = case object of
  (PDFIndirectObjectWithStream _ _ dict _    ) -> return dict
  (PDFObjectStream             _ _ dict _    ) -> return dict
  (PDFDictionary dict                        ) -> return dict
  (PDFIndirectObject _ _ (PDFDictionary dict)) -> return dict
  (PDFTrailer (PDFDictionary dict)           ) -> return dict
  _anyOtherObject                              -> throwError (NoDictionary "")

{-|
Retrieve a value from a PDF object's dictionary by key.

Works transparently on any PDF object containing a dictionary: 'PDFDictionary',
'PDFIndirectObjectWithStream', 'PDFObjectStream', 'PDFIndirectObject' (when
containing a dictionary), 'PDFTrailer', and 'PDFXRefStream'.

__Parameters:__

- The dictionary key name to look up
- The PDF object to search

__Returns:__ 'Just' the PDF object value if the key exists, or 'Nothing' if the
key is not found or the object has no dictionary.
-}
getValue
  :: Logging m
  => ByteString -- ^ Key of the value to retrieve
  -> PDFObject
  -> PDFWork m (Maybe PDFObject)
getValue name object = case object of
  (PDFDictionary dict                        ) -> return $ dict Map.!? name
  (PDFIndirectObjectWithStream _ _ dict _    ) -> return $ dict Map.!? name
  (PDFObjectStream             _ _ dict _    ) -> return $ dict Map.!? name
  (PDFIndirectObject _ _ (PDFDictionary dict)) -> return $ dict Map.!? name
  (PDFTrailer (PDFDictionary dict)           ) -> return $ dict Map.!? name
  (PDFXRefStream _ _ dict _                  ) -> return $ dict Map.!? name
  _anyOtherObject                              -> return Nothing

{-|
Retrieve a value from a PDF object's dictionary, with a default fallback.

Same as 'getValue' but if the key is not found, returns the provided default
value instead of 'Nothing'.

__Parameters:__

- The dictionary key name to look up
- A default PDF object to return if the key is not found
- The PDF object to search

__Returns:__ 'Just' the value if found, or 'Just' the default value if not
found, or 'Nothing' if the object has no dictionary.
-}
getValueDefault
  :: Logging m
  => ByteString -- ^ Key of the value to retrieve
  -> PDFObject
  -> PDFObject
  -> PDFWork m (Maybe PDFObject)
getValueDefault name defaultValue object = getValue name object >>= \case
  Just value -> return $ Just value
  Nothing    -> return $ Just defaultValue

{-|
Set or insert a key-value pair in a PDF object's dictionary.

Adds or updates a dictionary entry in any PDF object containing a dictionary. If
the object has no dictionary, it is returned unchanged.

__Parameters:__

- The dictionary key to set
- The PDF object value to assign
- The PDF object to modify

__Returns:__ The modified PDF object with the updated dictionary entry.
-}
setValue
  :: Logging m
  => ByteString -- ^ The key in a dictionary
  -> PDFObject -- ^ The value
  -> PDFObject -- ^ The PDFObject to modify
  -> PDFWork m PDFObject
setValue name value object = case object of
  (PDFDictionary dict) -> return $ PDFDictionary (Map.insert name value dict)
  (PDFIndirectObjectWithStream num gen dict stream) ->
    return $ PDFIndirectObjectWithStream num
                                         gen
                                         (Map.insert name value dict)
                                         stream
  (PDFObjectStream num gen dict stream) ->
    return $ PDFObjectStream num gen (Map.insert name value dict) stream
  (PDFIndirectObject num gen (PDFDictionary dict)) ->
    return
      $ PDFIndirectObject num gen (PDFDictionary (Map.insert name value dict))
  (PDFTrailer (PDFDictionary dict)) ->
    return $ PDFTrailer (PDFDictionary (Map.insert name value dict))
  (PDFXRefStream num gen dict stream) ->
    return $ PDFXRefStream num gen (Map.insert name value dict) stream
  _anyOtherObject -> return object

{-|
Delete a key from a PDF object's dictionary.

Removes a dictionary entry from any PDF object containing a dictionary. If the
key does not exist, the object is returned unchanged. If the object has no
dictionary, it is returned unchanged.

__Parameters:__

- The dictionary key to remove
- The PDF object to modify

__Returns:__ The modified PDF object with the specified key deleted.
-}
removeValue
  :: Logging m
  => ByteString -- ^ The key in a dictionary to remove
  -> PDFObject -- ^ The PDFObject to modify
  -> PDFWork m PDFObject
removeValue name object = case object of
  (PDFDictionary dict) -> return $ PDFDictionary (Map.delete name dict)
  (PDFIndirectObjectWithStream num gen dict stream) ->
    return $ PDFIndirectObjectWithStream num gen (Map.delete name dict) stream
  (PDFObjectStream num gen dict stream) ->
    return $ PDFObjectStream num gen (Map.delete name dict) stream
  (PDFIndirectObject num gen (PDFDictionary dict)) ->
    return $ PDFIndirectObject num gen (PDFDictionary (Map.delete name dict))
  (PDFTrailer (PDFDictionary dict)) ->
    return $ PDFTrailer (PDFDictionary (Map.delete name dict))
  (PDFXRefStream num gen dict stream) ->
    return $ PDFXRefStream num gen (Map.delete name dict) stream
  _anyOtherObject -> return object

{-|
Conditionally set a value in a PDF object's dictionary using a Maybe value.

If the value is 'Nothing', the object is returned unchanged. If the value is
'Just' something, 'setValue' is called to set it in the dictionary.

__Parameters:__

- The dictionary key
- An optional PDF object value
- The PDF object to modify

__Returns:__ The original object if value is 'Nothing', or the modified object
with the new entry if value is 'Just'.
-}
setMaybe
  :: Logging m
  => ByteString -- ^ The key in a dictionary
  -> Maybe PDFObject -- ^ The `Maybe` value
  -> PDFObject
  -> PDFWork m PDFObject
setMaybe _    Nothing      object = return object
setMaybe name (Just value) object = setValue name value object

{-|
Update or remove a dictionary entry based on a Maybe value.

If the value is 'Nothing', the entry is deleted using 'removeValue'. If the
value is 'Just' something, the entry is set using 'setValue'.

This combines deletion and insertion into a single operation for conditional
dictionary updates.

__Parameters:__

- The dictionary key
- An optional PDF object value
- The PDF object to modify

__Returns:__ The modified object with the entry removed (if 'Nothing') or
updated (if 'Just').
-}
updateValue
  :: Logging m
  => ByteString -- ^ The key in a dictionary
  -> Maybe PDFObject -- ^ The `Maybe` value
  -> PDFObject
  -> PDFWork m PDFObject
updateValue name Nothing      object = removeValue name object
updateValue name (Just value) object = setValue name value object

{-|
Replace the stream data in a PDF object and update its Length.

Sets new stream data for objects that contain streams. Automatically computes
and updates the 'Length' dictionary entry to reflect the new stream size in
bytes.

Only works on objects with streams: 'PDFIndirectObjectWithStream',
'PDFObjectStream', and 'PDFXRefStream'. Other objects are returned unchanged.

__Parameters:__

- The new stream bytestring data
- The PDF object to modify

__Returns:__ The modified object with new stream data and updated Length entry.
-}
setStream :: Logging m => ByteString -> PDFObject -> PDFWork m PDFObject
setStream newStream object = case object of
  (PDFIndirectObjectWithStream number revision dict _) ->
    setValue "Length" newLength
             (PDFIndirectObjectWithStream number revision dict newStream)
  (PDFObjectStream number revision dict _) -> do
    setValue "Length" newLength (PDFObjectStream number revision dict newStream)
  (PDFXRefStream number revision dict _) -> do
    setValue "Length" newLength (PDFXRefStream number revision dict newStream)
  _anyOtherObject -> return object
 where
  newLength :: PDFObject
  newLength = mkPDFNumber . BS.length $ newStream

{-|
Replace stream data and update both Length and Length1 entries.

Like 'setStream', but also sets the 'Length1' dictionary entry to record the
uncompressed size. This is used for streams with FlateDecode or other filters
where the original uncompressed length needs to be preserved.

__Parameters:__

- The uncompressed (original) size in bytes
- The new stream data (compressed or encoded)
- The PDF object to modify

__Returns:__ The modified object with new stream data and updated Length and
Length1 entries.
-}
setStream1
  :: Logging m
  => Int
  -> ByteString
  -> PDFObject
  -> PDFWork m PDFObject
setStream1 uncompressedLength newStream object =
  setStream newStream object >>=
    setValue "Length1" (mkPDFNumber uncompressedLength)

{-|
Embed an object into a `PDFObject`.

If the object is a `PDFDictionary`, its will be embedded in:

- `PDFIndirectObjectWithStream`
- `PDFObjectStream`
- `PDFDictionary`
- `PDFTrailer`
- `PDFArray` (object is appended to the existing list)

If the object is a `PDFArray`, it will be embedded in:

- `PDFArray` (object replaces the existing list)
- `PDFIndirectObject`

These `PDFObject` cannot be embedded in other `PDFObject`:

- `PDFComment`
- `PDFTrailer`
- `PDFVersion`
- `PDFEndOfFile`
- `PDFIndirectObject`
- `PDFIndirectObjectWithStream`
- `PDFObjectStream`
- `PDFXRef`
- `PDFTrailer`
- `PDFStartXRef`

-}
embedObject :: Logging m => PDFObject -> PDFObject -> PDFWork m PDFObject
embedObject toEmbed@(PDFDictionary dict) object = case object of
  (PDFIndirectObjectWithStream num gen _ stream) ->
    return $ PDFIndirectObjectWithStream num gen dict stream
  (PDFObjectStream num gen _ stream) ->
    return $ PDFObjectStream num gen dict stream
  (PDFIndirectObject num gen _) ->
    return $ PDFIndirectObject num gen (PDFDictionary dict)
  (PDFDictionary _) -> return $ PDFDictionary dict
  (PDFTrailer    _) -> return $ PDFTrailer (PDFDictionary dict)
  _anyOtherObject   -> cannotEmbed toEmbed object

embedObject toEmbed@(PDFArray items) object = case object of
  (PDFArray _) -> return $ PDFArray items
  (PDFIndirectObject num gen _) ->
    return $ PDFIndirectObject num gen (PDFArray items)
  _anyOtherObject -> cannotEmbed toEmbed object
embedObject toEmbed@PDFComment{}        object = cannotEmbed toEmbed object
embedObject toEmbed@PDFTrailer{}        object = cannotEmbed toEmbed object
embedObject toEmbed@PDFVersion{}        object = cannotEmbed toEmbed object
embedObject toEmbed@PDFEndOfFile{}      object = cannotEmbed toEmbed object
embedObject toEmbed@PDFIndirectObject{} object = cannotEmbed toEmbed object
embedObject toEmbed@PDFIndirectObjectWithStream{} object =
  cannotEmbed toEmbed object
embedObject toEmbed@PDFObjectStream{} object = cannotEmbed toEmbed object
embedObject toEmbed@PDFXRef{}         object = cannotEmbed toEmbed object
embedObject toEmbed@PDFStartXRef{}    object = cannotEmbed toEmbed object
embedObject toEmbed (PDFIndirectObject num gen _) =
  return $ PDFIndirectObject num gen toEmbed
embedObject toEmbed object = cannotEmbed toEmbed object

{-|
Throw an error indicating an object cannot be embedded in another.

Internal helper function for 'embedObject' that produces a consistent error
message when an invalid embedding is attempted.

__Parameters:__

- The object that cannot be embedded
- The container object it cannot be embedded into

__Fails:__ Always throws 'UnifiedError' 'InvalidObjectToEmbed' with a detailed
error message.
-}
cannotEmbed :: Logging m => PDFObject -> PDFObject -> PDFWork m PDFObject
cannotEmbed source destination = throwError
  (InvalidObjectToEmbed
    (show source ++ " cannot be embedded in " ++ show destination)
  )

{-|
Execute a monadic query on a PDF object, catching errors and converting to
Maybe.

Runs a query function that returns 'Maybe' in the 'PDFWork' monad, capturing the
result and suppressing any errors that occur during execution. Useful for
optional queries where failure should simply return 'Nothing' rather than
propagating an error.

__Parameters:__

- A query function that returns 'Maybe PDFObject' in 'PDFWork' 'Identity'
- The PDF object to query

__Returns:__ 'Just' the value if the query succeeds and returns 'Just', or
'Nothing' if the query fails or explicitly returns 'Nothing'.
-}
maybeQuery
  :: (PDFObject -> PDFWork Identity (Maybe PDFObject))
  -> PDFObject
  -> Maybe PDFObject
maybeQuery fn object =
  case runIdentity . runExceptT $ evalStateT (fn object) emptyWorkData of
    Right value     -> value
    Left  _anyError -> Nothing
