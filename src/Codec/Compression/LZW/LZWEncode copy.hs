{-|
This module implements the LZW uncompress alfgorithm as used in PDF file
(and also TIFF)
-}
module Codec.Compression.LZW.LZWEncode
  ( compress
  ) where

import Codec.Compression.LZW.LZWDictionary
    ( Dictionary
    , MonadDictionary (getDictionary, putDictionary)
    , addWord
    , clearTableMarker
    , dictionaryLength
    , dictionaryLengthM
    , endOfDataMarker
    , getIndexM
    , newDictionary
    )

import Control.Monad.State.Lazy (State, get, put, runState)

import Data.BitsSlice (BitsSlice, ToBitsSlice (rToBitsSlice), toByteString)
import Data.ByteString qualified as BS
import Data.Kind (Type)

import Util.UnifiedError (UnifiedError)

type EncodeStep :: Type
data EncodeStep = EncodeStep
  { esCurrentWord  :: BS.ByteString
  , esCurrentIndex :: Int
  , esBitsPerCode  :: Int
  , esDictionary   :: Dictionary
  }
  deriving stock (Eq, Show)

instance MonadDictionary (State EncodeStep) where
  getDictionary :: State EncodeStep Dictionary
  getDictionary = esDictionary <$> get

  putDictionary :: Dictionary -> State EncodeStep ()
  putDictionary value = get >>= \step -> put $ step { esDictionary = value }

initialEncodeStep :: EncodeStep
initialEncodeStep = EncodeStep
  { esCurrentWord = ""
  , esCurrentIndex = 0
  , esBitsPerCode = 9
  , esDictionary  = newDictionary
  }

setCurrentWord :: BS.ByteString -> State EncodeStep ()
setCurrentWord value = get >>= \step -> put $ step { esCurrentWord = value }

setCurrentIndex :: Int -> State EncodeStep ()
setCurrentIndex value = get >>= \step -> put $ step { esCurrentIndex = value }

processEncode :: BS.ByteString -> State EncodeStep BitsSlice
processEncode "" = do
  -- No more data to process, return the current index and the end of data
  -- marker.
  step <- get
  return $ rToBitsSlice (esBitsPerCode step) (esCurrentIndex step)
        <> rToBitsSlice (esBitsPerCode step) endOfDataMarker

processEncode stream = do
  step <- get
  let currentWord = esCurrentWord step
      nextByte = BS.head stream
      nextWord = BS.snoc currentWord nextByte

  getIndexM nextWord >>= \case
    Just wordIndex -> do
      setCurrentWord nextWord
      setCurrentIndex wordIndex
      processEncode (BS.tail stream)

    Nothing -> do
      dictLength <- dictionaryLengthM
      dictionary <- getDictionary
      let bitsPerCode = case dictLength of
                          511  -> 10
                          1023 -> 11
                          2047 -> 12
                          4095 -> 9
                          _    -> esBitsPerCode step

          dictionary' = if dictLength == 4095
                          then addWord nextWord newDictionary
                          else addWord nextWord dictionary

      let wordIndex = rToBitsSlice (esBitsPerCode step) (esCurrentIndex step)

      put $ step { esCurrentWord  = BS.singleton nextByte
                 , esCurrentIndex = fromIntegral nextByte
                 , esDictionary   = dictionary'
                 , esBitsPerCode  = bitsPerCode
                 }

      remains <- processEncode (BS.tail stream)
      return $ wordIndex <> if dictionaryLength dictionary' == 4095
            then rToBitsSlice (esBitsPerCode step) clearTableMarker <> remains
            else remains

{-|
Compress a strict bytestring using the LZW algorithm as described
in the PDF specifications.

It may return errors on invalid codes.
-}
compress
  :: BS.ByteString -- ^ A strict bytestring
  -> Either UnifiedError BS.ByteString
     -- ^ The compressed bytestring or an error
compress stream = do
  let (output, _anyError) = runState (processEncode stream) initialEncodeStep
  Right . toByteString $ rToBitsSlice 9 clearTableMarker <> output

