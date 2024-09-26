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
    , dictionaryLengthM
    , endOfDataMarker
    , getIndexM
    , newDictionary
    )

import Control.Monad.State.Lazy (State, get, put, runState)

import Data.BitsArray (BitsArray, appendBits, newBitsArray, toByteString)
import Data.ByteString qualified as BS
import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.UnifiedError (UnifiedError)

type EncodeStep :: Type
data EncodeStep = EncodeStep
  { esCurrentWord  :: !BS.ByteString
  , esCurrentIndex :: !Int
  , esBitsPerCode  :: !Int
  , esDictionary   :: !Dictionary
  , esOutput       :: !BitsArray
  }
  deriving stock (Eq, Show)

instance MonadDictionary (State EncodeStep) where
  getDictionary :: State EncodeStep Dictionary
  getDictionary = esDictionary <$> get

  putDictionary :: Dictionary -> State EncodeStep ()
  putDictionary value = get >>= \step -> put $ step { esDictionary = value }

initialEncodeStep :: Int -> EncodeStep
initialEncodeStep maxBytes = EncodeStep
  { esCurrentWord  = ""
  , esCurrentIndex = 0
  , esBitsPerCode  = 9
  , esDictionary   = newDictionary
  , esOutput       = appendBits 9 clearTableMarker (newBitsArray maxBytes)
  }

setCurrentWord :: BS.ByteString -> State EncodeStep ()
setCurrentWord value = get >>= \step -> put $ step { esCurrentWord = value }

setCurrentIndex :: Int -> State EncodeStep ()
setCurrentIndex value = get >>= \step -> put $ step { esCurrentIndex = value }

setBitsPerCode :: Int -> State EncodeStep ()
setBitsPerCode value = get >>= \step -> put $ step { esBitsPerCode = value }

outputCode :: Integral a => a -> State EncodeStep ()
outputCode value = do
  step <- get
  let output = appendBits (esBitsPerCode step) value (esOutput step)
  put $ step { esOutput = output }

processEncode :: BS.ByteString -> State EncodeStep BitsArray
processEncode "" = do
  -- No more data to process, return the current index and the end of data
  -- marker.
  get >>= outputCode . esCurrentIndex
  outputCode endOfDataMarker
  get <&> esOutput

processEncode stream = do
  step <- get
  let currentWord = esCurrentWord step
      nextByte = BS.head stream
      nextWord = BS.snoc currentWord nextByte

  getIndexM nextWord >>= \case
    -- The word is already in the dictionary, keep going.
    Just wordIndex -> do
      setCurrentWord nextWord
      setCurrentIndex wordIndex

    -- The word is not in the dictionary, add it and output the code for the
    -- last known word.
    Nothing -> do
      outputCode (esCurrentIndex step)

      dictLength <- dictionaryLengthM
      dictionary <- getDictionary

      if dictLength == 4096
        then do
          outputCode clearTableMarker
          putDictionary newDictionary
        else do
          putDictionary (addWord nextWord dictionary)

      newDictLength <- dictionaryLengthM
      setBitsPerCode (case newDictLength of
                          512  -> 10
                          1024 -> 11
                          2048 -> 12
                          258  -> 9
                          259  -> 9
                          _    -> esBitsPerCode step)

      setCurrentWord $ BS.singleton nextByte
      setCurrentIndex (fromIntegral nextByte)

  processEncode (BS.tail stream)

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
  let (output, _anyError) = runState (processEncode stream)
                                     (initialEncodeStep (BS.length stream * 2 + 16))
  Right . toByteString $ output

