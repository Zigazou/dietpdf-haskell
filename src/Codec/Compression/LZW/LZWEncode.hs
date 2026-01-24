{-|
Implements LZW compression as used in PDF (and also TIFF).

This encoder maintains an evolving dictionary of byte strings, emits variable-
width codes (starting at 9 bits), and increases the code width at standard
thresholds (512 → 10 bits, 1024 → 11 bits, 2048 → 12 bits). It inserts a
clear-table marker at initialization and when the dictionary reaches 4096
entries, and appends an end-of-data marker when input is exhausted.
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
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Fallible (Fallible)
import Data.Functor ((<&>))
import Data.Kind (Type)

{-|
Internal encoder state.

Fields:

* 'esCurrentWord'  — the current word under construction (last known match).
* 'esCurrentIndex' — code index of the current word.
* 'esBitsPerCode'  — current code width in bits (9..12).
* 'esDictionary'   — current LZW dictionary.
* 'esOutput'       — bit accumulator for emitted codes.
-}
type EncodeStep :: Type
data EncodeStep = EncodeStep
  { esCurrentWord  :: !ByteString -- ^ Current word under construction
  , esCurrentIndex :: !Int -- ^ Code index of the current word
  , esBitsPerCode  :: !Int -- ^ Current bits-per-code
  , esDictionary   :: !Dictionary -- ^ Current LZW dictionary
  , esOutput       :: !BitsArray -- ^ Accumulated output bitstream
  }
  deriving stock (Eq, Show)

instance MonadDictionary (State EncodeStep) where
  {-
  Read the current dictionary from the encoder state.
  -}
  getDictionary :: State EncodeStep Dictionary
  getDictionary = esDictionary <$> get

  {-
  Replace the current dictionary in the encoder state.
  -}
  putDictionary :: Dictionary -> State EncodeStep ()
  putDictionary value = get >>= \step -> put $ step { esDictionary = value }

{-|
Construct the initial encoder state for a given maximum output byte budget.

Starts with a fresh dictionary, 9-bit codes, and emits the clear-table marker
as the first code in the output stream.
-}
initialEncodeStep :: Int -> EncodeStep
initialEncodeStep maxBytes = EncodeStep
  { esCurrentWord  = ""
  , esCurrentIndex = 0
  , esBitsPerCode  = 9
  , esDictionary   = newDictionary
  , esOutput       = appendBits 9 clearTableMarker (newBitsArray maxBytes)
  }

{-|
Set the current word tracked by the encoder.
-}
setCurrentWord :: ByteString -> State EncodeStep ()
setCurrentWord value = get >>= \step -> put $ step { esCurrentWord = value }

{-|
Set the current word's code index.
-}
setCurrentIndex :: Int -> State EncodeStep ()
setCurrentIndex value = get >>= \step -> put $ step { esCurrentIndex = value }

{-|
Update the current bits-per-code setting (9..12).
-}
setBitsPerCode :: Int -> State EncodeStep ()
setBitsPerCode value = get >>= \step -> put $ step { esBitsPerCode = value }

{-|
Append a code to the output bitstream using the current code width.
-}
outputCode :: Integral a => a -> State EncodeStep ()
outputCode value = do
  step <- get
  let output = appendBits (esBitsPerCode step) value (esOutput step)
  put $ step { esOutput = output }

{-|
Process the input stream, updating the dictionary and emitting codes.

Algorithm:

* Extend the current word with the next byte and look it up in the dictionary.
* If present, continue accumulating bytes; update the current word/index.
* If absent, emit the code for the last known word, add the new word to the
  dictionary (or clear at length 4096), adjust bits-per-code at standard
  thresholds, and restart accumulation from the last byte.
* On end of input, emit the last word code and the end-of-data marker.
-}
processEncode :: ByteString -> State EncodeStep BitsArray
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
in the PDF specification.

Returns the compressed bytes on success.
-}
compress
  :: ByteString -- ^ A strict bytestring
  -> Fallible ByteString -- ^ The compressed bytestring or an error
compress stream = do
  let (output, _anyError) = runState (processEncode stream)
                                     (initialEncodeStep (BS.length stream * 2 + 16))
  Right . toByteString $ output

