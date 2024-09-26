{-|
This module implements the LZW uncompress alfgorithm as used in PDF file
(and also TIFF)
-}
module Codec.Compression.LZW.LZWDecode
  ( decompress
  ) where

import Codec.Compression.LZW.LZWDictionary
    ( Dictionary
    , MonadDictionary (getDictionary, putDictionary)
    , addWordM
    , clearTableMarker
    , dictionaryLength
    , dictionaryLengthM
    , endOfDataMarker
    , getWordM
    , minBitsPerCode
    , newDictionary
    , resetDictionaryM
    , validBitsPerCode
    )

import Control.Monad.Loops (whileM)
import Control.Monad.State.Lazy (State, get, put, runState)

import Data.Bits (shiftL, shiftR, (.&.))
import Data.ByteString qualified as BS
import Data.Fallible (Fallible)
import Data.Kind (Type)
import Data.Maybe (isNothing)
import Data.UnifiedError
    ( UnifiedError (InternalError, NotEnoughBytes, ParseError)
    )

type BitsRange :: Type
data BitsRange = BitsRange
  { brByteMask0 :: Int
  , brByteMask1 :: Int
  , brByteMask2 :: Int
  , brShift     :: Int
  , brLength    :: Int
  }
  deriving stock (Eq, Show)

bitsSlices :: Int -> Int -> Maybe BitsRange
bitsSlices spanLength offset
  | validBitsPerCode spanLength = Just $ BitsRange
    { brByteMask0 = shiftR (mask .&. 0x00ff0000) 16
    , brByteMask1 = shiftR (mask .&. 0x0000ff00) 8
    , brByteMask2 = mask .&. 0x000000ff
    , brShift     = (16 - spanLength - offset) .&. 7
    , brLength    = if mask .&. 0x000000ff /= 0 then 3 else 2
    }
  | otherwise = Nothing
 where
  mask :: Int
  mask = shiftR (shiftL 0xffffffff (24 - spanLength) .&. 0x00ffffff) offset

type DecodeStep :: Type
data DecodeStep = DecodeStep
  { psPreviousWordIndex :: Int
  , psBitPosition       :: Int
  , psBitsPerCode       :: Int
  , psDictionary        :: Dictionary
  , psError             :: Maybe UnifiedError
  }
  deriving stock (Eq, Show)

failWith :: Monoid a => UnifiedError -> State DecodeStep (Maybe a) -> State DecodeStep a
failWith errorValue action = do
  result <- action
  case result of
    Just value -> return value
    Nothing    -> setError errorValue >> return mempty

lzwNextCode :: BS.ByteString -> State DecodeStep (Fallible Int)
lzwNextCode stream = do
  step <- get
  let bitPosition = psBitPosition step
      offset      = shiftR bitPosition 3
      bitNumber   = bitPosition .&. 7
      slice       = bitsSlices (psBitsPerCode step) bitNumber

  case slice of
    Nothing -> return $ Left InternalError
    Just (BitsRange byteMask0 byteMask1 byteMask2 shift spanLength) -> do
      let byte :: Int -> Int
          byte = fromIntegral . BS.index stream . (offset +)

          code :: Fallible Int
          code = if
            | offset > BS.length stream - spanLength -> Left
            $ NotEnoughBytes spanLength (BS.length stream - offset)
            | spanLength == 2 -> Right
              (shiftL (byte 0 .&. byteMask0) 8 + (byte 1 .&. byteMask1))
            | spanLength == 3 -> Right
              ( shiftL (byte 0 .&. byteMask0) 16
              + shiftL (byte 1 .&. byteMask1) 8
              + (byte 2 .&. byteMask2)
              )
            | otherwise -> Left InternalError

      nextBitPosition
      return (flip shiftR shift <$> code)

setPreviousWordIndex :: Int -> State DecodeStep ()
setPreviousWordIndex value =
  get >>= \step -> put $ step { psPreviousWordIndex = value }

instance MonadDictionary (State DecodeStep) where
  getDictionary :: State DecodeStep Dictionary
  getDictionary = psDictionary <$> get

  putDictionary :: Dictionary -> State DecodeStep ()
  putDictionary value = get >>= \step -> put $ step { psDictionary = value }

getPreviousWordIndex :: State DecodeStep Int
getPreviousWordIndex = psPreviousWordIndex <$> get

getBitsPerCode :: State DecodeStep Int
getBitsPerCode = psBitsPerCode <$> get

setBitsPerCode :: Int -> State DecodeStep ()
setBitsPerCode value = get >>= \step -> put $ step { psBitsPerCode = value }

resetBitsPerCode :: State DecodeStep ()
resetBitsPerCode = setBitsPerCode minBitsPerCode

updateBitsPerCode :: State DecodeStep ()
updateBitsPerCode = do
  bitsPerCode <- getBitsPerCode
  dictLength  <- dictionaryLengthM
  setBitsPerCode $ if
    | dictLength == 511  -> 10
    | dictLength == 1023 -> 11
    | dictLength == 2047 -> 12
    | otherwise          -> bitsPerCode

nextBitPosition :: State DecodeStep ()
nextBitPosition = do
  step <- get
  put $ step { psBitPosition = psBitPosition step + psBitsPerCode step }

setError :: UnifiedError -> State DecodeStep ()
setError value = get >>= \step -> put $ step { psError = Just value }

moreToDecode :: Int -> State DecodeStep Bool
moreToDecode totalBits = do
  step <- get
  let bitsRemaining = psBitPosition step < totalBits
      noError       = isNothing (psError step)
      notTheEnd     = psPreviousWordIndex step /= endOfDataMarker
  return (notTheEnd && noError && bitsRemaining)

processDecode :: BS.ByteString -> State DecodeStep BS.ByteString
processDecode stream = updateBitsPerCode >> lzwNextCode stream >>= \case
  Left  anError          -> setError anError >> return ""
  Right currentWordIndex -> do
    dictionary        <- getDictionary
    previousWordIndex <- getPreviousWordIndex

    if
      | currentWordIndex == endOfDataMarker -> do
        setPreviousWordIndex currentWordIndex
        return ""
      | currentWordIndex == clearTableMarker -> do
        setPreviousWordIndex currentWordIndex
        resetBitsPerCode
        resetDictionaryM
        return ""
      | previousWordIndex == clearTableMarker -> do
        setPreviousWordIndex currentWordIndex
        resetBitsPerCode
        resetDictionaryM
        failWith InternalError (getWordM currentWordIndex)
      | currentWordIndex < dictionaryLength dictionary -> do
        previousWord <- failWith InternalError (getWordM previousWordIndex)
        setPreviousWordIndex currentWordIndex
        currentWord <- failWith InternalError (getWordM currentWordIndex)
        addWordM (BS.snoc previousWord (BS.index currentWord 0))
        return currentWord
      | currentWordIndex == dictionaryLength dictionary -> do
        previousWord <- failWith InternalError (getWordM previousWordIndex)
        setPreviousWordIndex currentWordIndex
        let newWord = BS.snoc previousWord (BS.index previousWord 0)
        addWordM newWord
        return newWord
      | otherwise -> do
        setError $ ParseError
          ( ""
          , 0
          , concat
            [ "Code value is "
            , show currentWordIndex
            , " but dictionary length is "
            , show (dictionaryLength dictionary)
            ]
          )
        return ""

initialDecodeStep :: Int -> DecodeStep
initialDecodeStep first = DecodeStep
  { psPreviousWordIndex = first
  , psBitPosition       = 0
  , psBitsPerCode       = minBitsPerCode
  , psDictionary        = newDictionary
  , psError             = Nothing
  }

{-|
Decompress a strict bytestring compressed using the LZW algorithm as described
in the PDF specifications.

It may return errors on invalid codes.
-}
decompress
  :: BS.ByteString -- ^ A strict bytestring of at least 2 bytes
  -> Fallible BS.ByteString
     -- ^ The uncompressed bytestring or an error
decompress stream = do
  let (output, state) = runState
        (whileM (moreToDecode $ BS.length stream * 8) (processDecode stream))
        (initialDecodeStep . fromIntegral . BS.head $ stream)
  case psError state of
    Just anError -> Left anError
    Nothing      -> Right (BS.concat output)
