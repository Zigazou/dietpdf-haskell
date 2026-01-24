{-|
Dictionary utilities for LZW compression/decompression.

This module defines the 'Dictionary' type used to map codes to byte sequences
and vice versa, along with helpers to initialize/reset the table, query, and
extend it. It also provides a small stateclass, 'MonadDictionary', to thread a
dictionary through computations in a monadic context.

The initial dictionary created by 'newDictionary' contains entries for codes
@0..255@ (single-byte values), plus the special markers @256@ ('clearTableMarker')
and @257@ ('endOfDataMarker'). This layout follows common LZW usage in image
and PDF streams.
-}
module Codec.Compression.LZW.LZWDictionary
  ( Dictionary (Dictionary, dList, dMap)
  , MonadDictionary (getDictionary, putDictionary)
  , clearTableMarker
  , endOfDataMarker
  , newDictionary
  , validBitsPerCode
  , minBitsPerCode
  , maxBitsPerCode
  , addWord
  , getWord
  , getIndex
  , dictionaryLength
  , addWordM
  , getWordM
  , getIndexM
  , resetDictionaryM
  , dictionaryLengthM
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Functor ((<&>))
import Data.Kind (Constraint, Type)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified as SQ

{-|
Minimum and maximum number of bits per LZW code supported.

Typical LZW implementations in PDF use variable-length codes in the range
@9..12@ bits.
-}
minBitsPerCode, maxBitsPerCode :: Int
minBitsPerCode = 9
maxBitsPerCode = 12

{-|
Check whether a given bits-per-code value is within the supported range.

Returns 'True' when @bitsPerCode ∈ [minBitsPerCode..maxBitsPerCode]@.
-}
validBitsPerCode :: Int -> Bool
validBitsPerCode bitsPerCode =
  bitsPerCode >= minBitsPerCode && bitsPerCode <= maxBitsPerCode

{-|
LZW dictionary mapping between codes and byte sequences.

Fields:

* 'dList': sequence of byte strings indexed by code (0-based).
* 'dMap' : reverse lookup from byte string to code index.

Invariant: when extended via 'addWord', the new word is appended to 'dList'
and inserted in 'dMap' with the next available index.
-}
type Dictionary :: Type
data Dictionary = Dictionary
  { dList :: !(Seq ByteString) -- ^ Sequence of words indexed by code
  , dMap  :: !(Map ByteString Int) -- ^ Reverse mapping from word to code
  } deriving stock (Eq, Show)

{-|
Special LZW marker codes.

* 'clearTableMarker' (@256@): signal to reset/clear the dictionary.
* 'endOfDataMarker'  (@257@): signal end-of-data in a code stream.
-}
clearTableMarker, endOfDataMarker :: Int
clearTableMarker = 256
endOfDataMarker = 257

{-|
Create a fresh dictionary with single-byte entries @0..255@ and marker slots
for @256@ and @257@.

The list ('dList') contains one 'ByteString' per initial code: codes
@0..255@ map to their single-byte value; the marker codes occupy positions but
use empty byte strings as placeholders. The map ('dMap') is populated only for
@0..255@.
-}
newDictionary :: Dictionary
newDictionary = Dictionary
  { dList = SQ.fromList
      [ if item < 256 then BS.singleton (fromIntegral item) else BS.empty
      | item <- [0 .. endOfDataMarker]
      ]
  , dMap = Map.fromList
      [ (BS.singleton (fromIntegral item), item)
      | item <- [0 .. 255]
      ]
  }

{-|
Append a new word to the dictionary, assigning it the next available code.
-}
addWord :: ByteString -> Dictionary -> Dictionary
addWord value dictionary =
  let dictionaryWords = dList dictionary
      newIndex = SQ.length dictionaryWords
  in dictionary {
    dList = dictionaryWords SQ.|> value,
    dMap = Map.insert value newIndex (dMap dictionary)
  }

{-|
Lookup a word (byte string) by its code index.

Returns 'Nothing' if the index is out of bounds.
-}
getWord :: Int -> Dictionary -> Maybe ByteString
getWord = (. dList) . SQ.lookup

{-|
Lookup the code index for a given word (byte string).
-}
getIndex :: ByteString -> Dictionary -> Maybe Int
getIndex value = Map.lookup value . dMap

{-|
Current number of entries in the dictionary.
-}
dictionaryLength :: Dictionary -> Int
dictionaryLength = SQ.length . dList

{-|
Minimal stateclass for threading a 'Dictionary' through monadic code.

Provides 'getDictionary' to read the current dictionary and 'putDictionary' to
replace it.
-}
type MonadDictionary :: (Type -> Type) -> Constraint
class Monad m => MonadDictionary m where
  -- | Get the current LZW dictionary.
  getDictionary :: m Dictionary

  -- | Set the current LZW dictionary.
  putDictionary :: Dictionary -> m ()

{-|
Monadic variant of 'addWord'.
-}
addWordM :: MonadDictionary m => ByteString -> m ()
addWordM value = getDictionary >>= putDictionary . addWord value

{-|
Monadic variant of 'getWord'.
-}
getWordM :: MonadDictionary m => Int -> m (Maybe ByteString)
getWordM offset = getDictionary <&> getWord offset

{-|
Monadic variant of 'getIndex'.
-}
getIndexM :: MonadDictionary m => ByteString -> m (Maybe Int)
getIndexM value = getDictionary <&> getIndex value

{-|
Reset the monadic dictionary to 'newDictionary'.
-}
resetDictionaryM :: MonadDictionary m => m ()
resetDictionaryM = putDictionary newDictionary

{-|
Monadic variant of 'dictionaryLength'.
-}
dictionaryLengthM :: MonadDictionary m => m Int
dictionaryLengthM = getDictionary <&> dictionaryLength
