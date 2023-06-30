{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}

{-|
This module defines functions working on `StateT` monad with `PDFObject`.
-}
module Pdf.Object.State
  ( -- * Types
    FallibleComputation
  , ObjectComputation

    -- * Query
  , getValue
  , query
  , queryE
  , getStream
  , getDictionary
  , hasDictionaryS
  , hasKeyS
  , hasStreamS

    -- * Modify
  , setValue
  , (.=)
  , setMaybe
  , (?=)
  , update
  , updateE
  , setStream
  , setDictionary
  , embedObject
  , modifyObject

    -- * Check
  , ifObject
  , ifObjectElse
  ) where

import           Control.Monad                  ( when )
import           Control.Monad.State            ( StateT
                                                , evalState
                                                , evalStateT
                                                , execState
                                                , execStateT
                                                , get
                                                , lift
                                                , put
                                                )
import qualified Data.ByteString               as BS
import           Data.Functor                   ( (<&>) )
import qualified Data.Map.Strict               as Map
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFArray
                                                  , PDFComment
                                                  , PDFDictionary
                                                  , PDFEndOfFile
                                                  , PDFIndirectObject
                                                  , PDFIndirectObjectWithStream
                                                  , PDFNumber
                                                  , PDFObjectStream
                                                  , PDFStartXRef
                                                  , PDFTrailer
                                                  , PDFVersion
                                                  , PDFXRef
                                                  )
                                                , hasDictionary
                                                , hasKey
                                                , hasStream
                                                )
import           Util.Errors                    ( UnifiedError
                                                  ( InvalidObjectToEmbed
                                                  , NoDictionary
                                                  , NoStream
                                                  )
                                                , unifiedError
                                                )
import           Control.Monad.Identity         ( Identity )
import           Util.Dictionary                ( Dictionary )

{- |
A `FallibleComputation` is a computation running in a `State` monad, working on
`PDFObject` which can fail by returning a `UnifiedError`.

To use this type, you have to parameterize with the return type.

Example:

    getStream :: FallibleComputation BS.ByteString
-}
type FallibleComputation = StateT PDFObject (Either UnifiedError)

{- |
An `ObjectComputation` is a computation running in a `State` monad, working on
`PDFObject`.

This type needs two parameters:

- the first one is another monad (Either, Maybe…)
- the second one is the return type. You can use `()` if it returns nothing.

Example:
  getValue
    :: Monad m
    => BS.ByteString
    -> ObjectComputation m (Maybe PDFObject)

  type FallibleComputation = ObjectComputation (Either UnifiedError)
-}
type ObjectComputation = StateT PDFObject

{- |
Returns the stream embedded in a `PDFObject`.

If the object has no stream, a `UnifiedError` `NoStream` stops the evaluation
of the monad.

Only `PDFIndirectObjectWithStream` and `PDFObjectStream` have stream embedded.
-}
getStream :: FallibleComputation BS.ByteString
getStream = get >>= \case
  (PDFIndirectObjectWithStream _ _ _ stream) -> return stream
  (PDFObjectStream             _ _ _ stream) -> return stream
  _anyOtherObject                            -> unifiedError (NoStream "")

{- |
Returns the dictionary embedded in a `PDFObject`.

If the object has no dictionary, a `UnifiedError` `NoDictionary` stops the
evaluation of the monad.
-}
getDictionary :: FallibleComputation (Dictionary PDFObject)
getDictionary = get >>= \case
  (PDFIndirectObjectWithStream _ _ dict _    ) -> return dict
  (PDFObjectStream             _ _ dict _    ) -> return dict
  (PDFDictionary dict                        ) -> return dict
  (PDFIndirectObject _ _ (PDFDictionary dict)) -> return dict
  (PDFTrailer (PDFDictionary dict)           ) -> return dict
  _anyOtherObject                              -> unifiedError (NoDictionary "")

{- |
Get value in a dictionary from a `PDFObject`.

It works transparently for any `PDFObject` containing a dictionary:
`PDFDictionary`, `PDFIndirectObjectWithStream`, `PDFObjectStream`,
`PDFIndirectObject` (when embedding a `Dictionary`) and `PDFTrailer`.
-}
getValue
  :: Monad m
  => BS.ByteString -- ^ Key of the value to retrieve
  -> ObjectComputation m (Maybe PDFObject) -- ^ The `StateT` monad in which the
                                           -- `PDFObject` is contained
getValue name = get <&> \case
  (PDFDictionary dict                        ) -> dict Map.!? name
  (PDFIndirectObjectWithStream _ _ dict _    ) -> dict Map.!? name
  (PDFObjectStream             _ _ dict _    ) -> dict Map.!? name
  (PDFIndirectObject _ _ (PDFDictionary dict)) -> dict Map.!? name
  (PDFTrailer (PDFDictionary dict)           ) -> dict Map.!? name
  _anyOtherObject                              -> Nothing

{- |
Set value in a dictionary contained in a `PDFObject`.

If the object has no dictionary, it is ignored.
-}
setValue
  :: Monad m
  => BS.ByteString -- ^ The key in a dictionary
  -> PDFObject -- ^ The value
  -> ObjectComputation m () -- ^ The `StateT` monad in which to operate
setValue name value = get >>= \case
  (PDFDictionary dict) -> put (PDFDictionary (Map.insert name value dict))
  (PDFIndirectObjectWithStream num gen dict stream) -> put
    (PDFIndirectObjectWithStream num gen (Map.insert name value dict) stream)
  (PDFObjectStream num gen dict stream) ->
    put (PDFObjectStream num gen (Map.insert name value dict) stream)
  (PDFIndirectObject num gen (PDFDictionary dict)) ->
    put (PDFIndirectObject num gen (PDFDictionary (Map.insert name value dict)))
  (PDFTrailer (PDFDictionary dict)) ->
    put (PDFTrailer (PDFDictionary (Map.insert name value dict)))
  _anyOtherObject -> return ()

{- |
Set value (maybe) in a dictionary contained in a `PDFObject`.

When the value is `Nothing`, this function does nothing.
When the value is `Just` something, the entry is set in the dictionary using
the `setValue` functin.
-}
setMaybe
  :: Monad m
  => BS.ByteString -- ^ The key in a dictionary
  -> Maybe PDFObject -- ^ The `Maybe` value
  -> ObjectComputation m () -- ^ The `StateT` monad in which to operate
setMaybe _    Nothing      = return ()
setMaybe name (Just value) = setValue name value

infixr 9 .=
{- |
A convenient alias for `setValue`.
-}
(.=)
  :: Monad m
  => BS.ByteString -- ^ The key in a dictionary
  -> PDFObject -- ^ The value
  -> ObjectComputation m () -- ^ The `StateT` monad in which to operate
(.=) = setValue

infixr 9 ?=
{- |
A convenient alias for `setMaybe`.
-}
(?=)
  :: Monad m
  => BS.ByteString -- ^ The key in a dictionary
  -> Maybe PDFObject -- ^ The `Maybe` value
  -> ObjectComputation m () -- ^ The `StateT` monad in which to operate
(?=) = setMaybe

{- |
Define the stream part of a `PDFObject` if it has one.

It also updates the Length entry in the associated `Dictionary`.

This function works only on `PDFIndirectObjectStream` and `PDFObjectStream`.

It has no effect on any other `PDFObject`.
-}
setStream :: Monad m => BS.ByteString -> ObjectComputation m ()
setStream newStream = get >>= \case
  (PDFIndirectObjectWithStream number revision dict _) -> do
    put (PDFIndirectObjectWithStream number revision dict newStream)
    "Length" .= newLength
  (PDFObjectStream number revision dict _) -> do
    put (PDFObjectStream number revision dict newStream)
    "Length" .= newLength
  _anyOtherObject -> return ()
 where
  newLength :: PDFObject
  newLength = PDFNumber . fromIntegral . BS.length $ newStream

{- |
Determine if `PDFObject` has a dictionary with a specific key into it.

This function is meant to be used with functions like `ifObject` or
`ifObjectElse`.
-}
hasKeyS :: Monad m => BS.ByteString -> ObjectComputation m Bool
hasKeyS = (`fmap` get) . hasKey

{- |
Determine if `PDFObject` has a dictionary with a specific key into it.

This function is meant to be used with functions like `ifObject` or
`ifObjectElse`.
-}
hasDictionaryS :: Monad m => ObjectComputation m Bool
hasDictionaryS = get <&> hasDictionary

{- |
Replace the `Dictionary` embedded in a `PDFObject`.

This function works on:

- `PDFIndirectObjectWithStream`
- `PDFObjectStream`
- `PDFIndirectObject` (only if it already has a `Dictionary`)
- `PDFDictionary`

Any other object will yield a `UnifiedError` `InvalidObjectToEmbed`, stopping
the evaluation of the monad.
-}
setDictionary :: Dictionary PDFObject -> FallibleComputation ()
setDictionary dict = get >>= \case
  (PDFIndirectObjectWithStream num gen _ stream) ->
    put (PDFIndirectObjectWithStream num gen dict stream)
  (PDFObjectStream num gen _ stream) ->
    put (PDFObjectStream num gen dict stream)
  (PDFIndirectObject num gen (PDFDictionary _)) ->
    put (PDFIndirectObject num gen (PDFDictionary dict))
  (PDFDictionary _) -> put (PDFDictionary dict)
  _anyOtherObject   -> unifiedError (InvalidObjectToEmbed "")

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
embedObject :: PDFObject -> FallibleComputation ()
embedObject object@(PDFDictionary dict) = get >>= \case
  (PDFIndirectObjectWithStream num gen _ stream) ->
    put (PDFIndirectObjectWithStream num gen dict stream)
  (PDFObjectStream num gen _ stream) ->
    put (PDFObjectStream num gen dict stream)
  (PDFIndirectObject num gen _) ->
    put (PDFIndirectObject num gen (PDFDictionary dict))
  (PDFDictionary _) -> put (PDFDictionary dict)
  (PDFTrailer    _) -> put (PDFTrailer (PDFDictionary dict))
  _anyOtherObject   -> cannotEmbed object
embedObject object@(PDFArray items) = get >>= \case
  (PDFArray _) -> put (PDFArray items)
  (PDFIndirectObject num gen _) ->
    put (PDFIndirectObject num gen (PDFArray items))
  _anyOtherObject -> cannotEmbed object
embedObject object@PDFComment{}                  = cannotEmbed object
embedObject object@PDFTrailer{}                  = cannotEmbed object
embedObject object@PDFVersion{}                  = cannotEmbed object
embedObject object@PDFEndOfFile{}                = cannotEmbed object
embedObject object@PDFIndirectObject{}           = cannotEmbed object
embedObject object@PDFIndirectObjectWithStream{} = cannotEmbed object
embedObject object@PDFObjectStream{}             = cannotEmbed object
embedObject object@PDFXRef{}                     = cannotEmbed object
embedObject object@PDFStartXRef{}                = cannotEmbed object
embedObject object                               = get >>= \case
  (PDFIndirectObject num gen (PDFDictionary _)) ->
    put (PDFIndirectObject num gen object)
  _anyOtherObject -> cannotEmbed object

cannotEmbed :: PDFObject -> FallibleComputation ()
cannotEmbed source = do
  destination <- get
  unifiedError
    (InvalidObjectToEmbed
      (show source ++ " cannot be embedded in " ++ show destination)
    )
{- |
Apply a function to modify an embedded object.

If the function returns a `UnifiedError`, the evaluation of the monad is
stopped.

To ignore an object, the function just has to return it unchanged.
-}
modifyObject
  :: (PDFObject -> Either UnifiedError PDFObject) -> FallibleComputation ()
modifyObject fn = get >>= \case
  (PDFIndirectObjectWithStream _ _ dict _) ->
    lift (fn (PDFDictionary dict)) >>= embedObject
  (PDFObjectStream _ _ dict _) ->
    lift (fn (PDFDictionary dict)) >>= embedObject
  (PDFIndirectObject _ _ object@PDFDictionary{}) ->
    lift (fn object) >>= embedObject
  object@PDFDictionary{} -> lift (fn object) >>= embedObject
  object                 -> unifiedError
    (InvalidObjectToEmbed ("No embedded object to modify " ++ show object))

{- |
Determine if `PDFObject` has a stream.

This function is meant to be used with functions like `ifObject` or
`ifObjectElse`.
-}
hasStreamS :: Monad m => ObjectComputation m Bool
hasStreamS = get <&> hasStream

{- |
Update an object using a `StateT` monad.

Any error while evaluating the monad will be ignored.
-}
update
  :: PDFObject -- ^ Object to update
  -> ObjectComputation Identity a -- ^ Computation
  -> PDFObject -- ^ The updated object
update = flip execState

{- |
Update an object using a `StateT` monad.

Any error while evaluating the monad will stop its evaluation.
-}
updateE
  :: PDFObject -- ^ Object to update
  -> FallibleComputation a -- ^ Computation
  -> Either UnifiedError PDFObject -- ^ The updated object or an error
updateE = flip execStateT

{- |
Query an object using a `StateT` monad.

Any error while evaluating the monad will be ignored.
-}
query
  :: PDFObject -- ^ The object to inspect
  -> ObjectComputation Identity a -- ^ Query
  -> a -- ^ The result
query = flip evalState

{- |
Query an object using a `StateT` monad.

Any error while evaluating the monad will stop its evaluation.
-}
queryE
  :: PDFObject -- ^ The object to inspect
  -> FallibleComputation a -- ^ Query
  -> Either UnifiedError a -- ^ The result or an error
queryE = flip evalStateT

{- |
An equivalent of the `when` function working directly with `StateT` and
`PDFObject`.
-}
ifObject
  :: Monad m
  => ObjectComputation m Bool -- ^ Condition
  -> ObjectComputation m () -- ^ Monad to evaluate if condition is met
  -> ObjectComputation m () -- ^ Result
ifObject = (. flip when) . (>>=)

{- |
An equivalent of if … then … else … working directly with `StateT` and
`PDFObject`.
-}
ifObjectElse
  :: Monad m
  => ObjectComputation m Bool -- ^ Condition
  -> ObjectComputation m () -- ^ Monad to evaluate if condition is met
  -> ObjectComputation m () -- ^ Monad to evaluate if condition is not met
  -> ObjectComputation m () -- ^ Result
ifObjectElse condition thenAction elseAction = do
  value <- condition
  if value then thenAction else elseAction
