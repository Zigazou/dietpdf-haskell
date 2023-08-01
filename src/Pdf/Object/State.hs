{-|
This module defines functions working on `StateT` monad with `PDFObject`.
-}
module Pdf.Object.State
  ( -- * Query
    getValue
  , getStream
  , getDictionary
  , maybeQuery

    -- * Modify
  , setValue
  , setMaybe
  , setStream
  , embedObject
  ) where

import qualified Data.ByteString               as BS
import qualified Data.Map.Strict               as Map
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFArray
                                                  , PDFComment
                                                  , PDFDictionary
                                                  , PDFEndOfFile
                                                  , PDFIndirectObject
                                                  , PDFIndirectObjectWithStream
                                                  , PDFObjectStream
                                                  , PDFStartXRef
                                                  , PDFTrailer
                                                  , PDFVersion
                                                  , PDFXRef
                                                  )
                                                , ToPDFNumber(mkPDFNumber)
                                                )
import           Util.UnifiedError              ( UnifiedError
                                                  ( InvalidObjectToEmbed
                                                  , NoDictionary
                                                  , NoStream
                                                  )
                                                , FallibleT
                                                )
import           Util.Dictionary                ( Dictionary )
import           Control.Monad.Trans.Except     ( throwE
                                                , runExceptT
                                                )
import           Control.Monad.Identity         ( Identity(runIdentity) )
import           Util.Logging                   ( Logging )

{- |
Returns the stream embedded in a `PDFObject`.

If the object has no stream, a `UnifiedError` `NoStream` stops the evaluation
of the monad.

Only `PDFIndirectObjectWithStream` and `PDFObjectStream` have stream embedded.
-}
getStream :: Logging m => PDFObject -> FallibleT m BS.ByteString
getStream object = case object of
  (PDFIndirectObjectWithStream _ _ _ stream) -> return stream
  (PDFObjectStream             _ _ _ stream) -> return stream
  _anyOtherObject                            -> throwE (NoStream "")

{- |
Returns the dictionary embedded in a `PDFObject`.

If the object has no dictionary, a `UnifiedError` `NoDictionary` stops the
evaluation of the monad.
-}
getDictionary :: Logging m => PDFObject -> FallibleT m (Dictionary PDFObject)
getDictionary object = case object of
  (PDFIndirectObjectWithStream _ _ dict _    ) -> return dict
  (PDFObjectStream             _ _ dict _    ) -> return dict
  (PDFDictionary dict                        ) -> return dict
  (PDFIndirectObject _ _ (PDFDictionary dict)) -> return dict
  (PDFTrailer (PDFDictionary dict)           ) -> return dict
  _anyOtherObject                              -> throwE (NoDictionary "")

{- |
Get value in a dictionary from a `PDFObject`.

It works transparently for any `PDFObject` containing a dictionary:
`PDFDictionary`, `PDFIndirectObjectWithStream`, `PDFObjectStream`,
`PDFIndirectObject` (when embedding a `Dictionary`) and `PDFTrailer`.
-}
getValue
  :: Logging m
  => BS.ByteString -- ^ Key of the value to retrieve
  -> PDFObject
  -> FallibleT m (Maybe PDFObject)
getValue name object = case object of
  (PDFDictionary dict                        ) -> return $ dict Map.!? name
  (PDFIndirectObjectWithStream _ _ dict _    ) -> return $ dict Map.!? name
  (PDFObjectStream             _ _ dict _    ) -> return $ dict Map.!? name
  (PDFIndirectObject _ _ (PDFDictionary dict)) -> return $ dict Map.!? name
  (PDFTrailer (PDFDictionary dict)           ) -> return $ dict Map.!? name
  _anyOtherObject                              -> return Nothing

{- |
Set value in a dictionary contained in a `PDFObject`.

If the object has no dictionary, it is ignored.
-}
setValue
  :: Logging m
  => BS.ByteString -- ^ The key in a dictionary
  -> PDFObject -- ^ The value
  -> PDFObject -- ^ The PDFObject to modify
  -> FallibleT m PDFObject
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
  _anyOtherObject -> return object

{- |
Set value (maybe) in a dictionary contained in a `PDFObject`.

When the value is `Nothing`, this function does nothing.
When the value is `Just` something, the entry is set in the dictionary using
the `setValue` functin.
-}
setMaybe
  :: Logging m
  => BS.ByteString -- ^ The key in a dictionary
  -> Maybe PDFObject -- ^ The `Maybe` value
  -> PDFObject
  -> FallibleT m PDFObject
setMaybe _    Nothing      object = return object
setMaybe name (Just value) object = setValue name value object

{- |
Define the stream part of a `PDFObject` if it has one.

It also updates the Length entry in the associated `Dictionary`.

This function works only on `PDFIndirectObjectStream` and `PDFObjectStream`.

It has no effect on any other `PDFObject`.
-}
setStream :: Logging m => BS.ByteString -> PDFObject -> FallibleT m PDFObject
setStream newStream object = case object of
  (PDFIndirectObjectWithStream number revision dict _) ->
    pure (PDFIndirectObjectWithStream number revision dict newStream)
      >>= setValue "Length" newLength
  (PDFObjectStream number revision dict _) -> do
    pure (PDFObjectStream number revision dict newStream)
      >>= setValue "Length" newLength
  _anyOtherObject -> return object
 where
  newLength :: PDFObject
  newLength = mkPDFNumber . BS.length $ newStream

{- |
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
embedObject :: Logging m => PDFObject -> PDFObject -> FallibleT m PDFObject
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

cannotEmbed :: Logging m => PDFObject -> PDFObject -> FallibleT m PDFObject
cannotEmbed source destination = throwE
  (InvalidObjectToEmbed
    (show source ++ " cannot be embedded in " ++ show destination)
  )

maybeQuery
  :: (PDFObject -> FallibleT Identity (Maybe PDFObject))
  -> PDFObject
  -> Maybe PDFObject
maybeQuery fn object = case runIdentity . runExceptT $ fn object of
  Right value     -> value
  Left  _anyError -> Nothing
