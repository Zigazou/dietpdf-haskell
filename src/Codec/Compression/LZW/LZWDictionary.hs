{-# LANGUAGE MultiParamTypeClasses #-}
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


import Data.ByteString qualified as BS
import Data.Functor ((<&>))
import Data.Kind (Constraint, Type)
import Data.Map qualified as Map
import Data.Sequence qualified as SQ

minBitsPerCode, maxBitsPerCode :: Int
minBitsPerCode = 9
maxBitsPerCode = 12

validBitsPerCode :: Int -> Bool
validBitsPerCode bitsPerCode =
  bitsPerCode >= minBitsPerCode && bitsPerCode <= maxBitsPerCode

type Dictionary :: Type
data Dictionary = Dictionary
  { dList :: !(SQ.Seq BS.ByteString)
  , dMap  :: !(Map.Map BS.ByteString Int)
  } deriving stock (Eq, Show)

clearTableMarker, endOfDataMarker :: Int
clearTableMarker = 256
endOfDataMarker = 257

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

addWord :: BS.ByteString -> Dictionary -> Dictionary
addWord value dictionary =
  let dictionaryWords = dList dictionary
      newIndex = SQ.length dictionaryWords
  in dictionary {
    dList = dictionaryWords SQ.|> value,
    dMap = Map.insert value newIndex (dMap dictionary)
  }

getWord :: Int -> Dictionary -> Maybe BS.ByteString
getWord = (. dList) . SQ.lookup

getIndex :: BS.ByteString -> Dictionary -> Maybe Int
getIndex value = Map.lookup value . dMap

dictionaryLength :: Dictionary -> Int
dictionaryLength = SQ.length . dList

type MonadDictionary :: (Type -> Type) -> Constraint
class Monad m => MonadDictionary m where
  getDictionary :: m Dictionary
  putDictionary :: Dictionary -> m ()

addWordM :: MonadDictionary m => BS.ByteString -> m ()
addWordM value = getDictionary >>= putDictionary . addWord value

getWordM :: MonadDictionary m => Int -> m (Maybe BS.ByteString)
getWordM offset = getDictionary <&> getWord offset

getIndexM :: MonadDictionary m => BS.ByteString -> m (Maybe Int)
getIndexM value = getDictionary <&> getIndex value

resetDictionaryM :: MonadDictionary m => m ()
resetDictionaryM = putDictionary newDictionary

dictionaryLengthM :: MonadDictionary m => m Int
dictionaryLengthM = getDictionary <&> dictionaryLength
