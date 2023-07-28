{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-|
This module implements the LZW uncompress alfgorithm as used in PDF file
(and also TIFF)
-}
module Codec.Compression.LZW
  ( decompress
  ) where

import qualified Data.ByteString               as BS
import           Data.Bits                      ( (.&.)
                                                , shiftL
                                                , shiftR
                                                )
import qualified Data.Sequence                 as SQ
import           Util.UnifiedError                    ( UnifiedError
                                                  ( ParseError
                                                  , InternalError
                                                  , NotEnoughBytes
                                                  )
                                                )

import           Control.Monad.State.Lazy       ( State()
                                                , get
                                                , put
                                                , runState
                                                )
import           Control.Monad.Loops            ( whileM )
import           Data.Maybe                     ( isNothing )

data BitsSlice = BitsSlice
  { bslByteMask0 :: Int
  , bslByteMask1 :: Int
  , bslByteMask2 :: Int
  , bslShift     :: Int
  , bslLength    :: Int
  }
  deriving stock (Eq, Show)

minBitsPerCode, maxBitsPerCode :: Int
minBitsPerCode = 9
maxBitsPerCode = 12

validBitsPerCode :: Int -> Bool
validBitsPerCode bitsPerCode =
  bitsPerCode >= minBitsPerCode && bitsPerCode <= maxBitsPerCode

bitsSlices :: Int -> Int -> Maybe BitsSlice
bitsSlices spanLength offset
  | validBitsPerCode spanLength = Just $ BitsSlice
    { bslByteMask0 = shiftR (mask .&. 0x00ff0000) 16
    , bslByteMask1 = shiftR (mask .&. 0x0000ff00) 8
    , bslByteMask2 = mask .&. 0x000000ff
    , bslShift     = (16 - spanLength - offset) .&. 7
    , bslLength    = if mask .&. 0x000000ff /= 0 then 3 else 2
    }
  | otherwise = Nothing
 where
  mask :: Int
  mask = shiftR (shiftL 0xffffffff (24 - spanLength) .&. 0x00ffffff) offset

type Dictionary = SQ.Seq BS.ByteString

clearTableMarker, endOfDataMarker :: Int
clearTableMarker = 256
endOfDataMarker = 257

newDictionary :: Dictionary
newDictionary = SQ.fromList
  [ if item < 256 then BS.singleton (fromIntegral item) else BS.empty
  | item <- [0 .. endOfDataMarker]
  ]

data ProcessStep = ProcessStep
  { psPreviousWordIndex :: Int
  , psBitPosition       :: Int
  , psBitsPerCode       :: Int
  , psDictionary        :: Dictionary
  , psError             :: Maybe UnifiedError
  }
  deriving stock (Eq, Show)

lzwNextCode :: BS.ByteString -> State ProcessStep (Either UnifiedError Int)
lzwNextCode stream = do
  step <- get
  let bitPosition = psBitPosition step
      offset      = shiftR bitPosition 3
      bitNumber   = bitPosition .&. 7
      slice       = bitsSlices (psBitsPerCode step) bitNumber

  case slice of
    Nothing -> return $ Left InternalError
    Just (BitsSlice byteMask0 byteMask1 byteMask2 shift spanLength) -> do
      let byte :: Int -> Int
          byte = fromIntegral . BS.index stream . (offset +)

          code :: Either UnifiedError Int
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

setPreviousWordIndex :: Int -> State ProcessStep ()
setPreviousWordIndex value =
  get >>= \step -> put $ step { psPreviousWordIndex = value }

getDictionary :: State ProcessStep Dictionary
getDictionary = psDictionary <$> get

getPreviousWordIndex :: State ProcessStep Int
getPreviousWordIndex = psPreviousWordIndex <$> get

addWord :: BS.ByteString -> State ProcessStep ()
addWord value =
  get >>= \step -> put $ step { psDictionary = psDictionary step SQ.|> value }

getWord :: Int -> State ProcessStep BS.ByteString
getWord offset = do
  dictionary <- getDictionary
  if offset < SQ.length dictionary
    then return $ SQ.index dictionary offset
    else setError InternalError >> return ""

resetDictionary :: State ProcessStep ()
resetDictionary = get >>= \step -> put $ step { psDictionary = newDictionary }

getBitsPerCode :: State ProcessStep Int
getBitsPerCode = psBitsPerCode <$> get

setBitsPerCode :: Int -> State ProcessStep ()
setBitsPerCode value = get >>= \step -> put $ step { psBitsPerCode = value }

resetBitsPerCode :: State ProcessStep ()
resetBitsPerCode = setBitsPerCode minBitsPerCode

dictionaryLength :: State ProcessStep Int
dictionaryLength = SQ.length . psDictionary <$> get

updateBitsPerCode :: State ProcessStep ()
updateBitsPerCode = do
  bitsPerCode <- getBitsPerCode
  dictLength  <- dictionaryLength
  setBitsPerCode $ if
    | dictLength == 511  -> 10
    | dictLength == 1023 -> 11
    | dictLength == 2047 -> 12
    | otherwise          -> bitsPerCode

nextBitPosition :: State ProcessStep ()
nextBitPosition = do
  step <- get
  put $ step { psBitPosition = psBitPosition step + psBitsPerCode step }

setError :: UnifiedError -> State ProcessStep ()
setError value = get >>= \step -> put $ step { psError = Just value }

moreToDecode :: Int -> State ProcessStep Bool
moreToDecode totalBits = do
  step <- get
  let bitsRemaining = psBitPosition step < totalBits
      noError       = isNothing (psError step)
      notTheEnd     = psPreviousWordIndex step /= endOfDataMarker
  return (notTheEnd && noError && bitsRemaining)

processCode :: BS.ByteString -> State ProcessStep BS.ByteString
processCode stream = updateBitsPerCode >> lzwNextCode stream >>= \case
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
        resetDictionary
        return ""
      | previousWordIndex == clearTableMarker -> do
        setPreviousWordIndex currentWordIndex
        resetBitsPerCode
        resetDictionary
        getWord currentWordIndex
      | currentWordIndex < SQ.length dictionary -> do
        previousWord <- getWord previousWordIndex
        setPreviousWordIndex currentWordIndex
        currentWord <- getWord currentWordIndex
        addWord (BS.snoc previousWord (BS.index currentWord 0))
        return currentWord
      | currentWordIndex == SQ.length dictionary -> do
        previousWord <- getWord previousWordIndex
        setPreviousWordIndex currentWordIndex
        let newWord = BS.snoc previousWord (BS.index previousWord 0)
        addWord newWord
        return newWord
      | otherwise -> do
        setError $ ParseError
          ( ""
          , 0
          , concat
            [ "Code value is  "
            , show currentWordIndex
            , " but dictionary length is "
            , show (SQ.length dictionary)
            ]
          )
        return ""

initialStep :: Int -> ProcessStep
initialStep first = ProcessStep first 0 minBitsPerCode newDictionary Nothing

{-|
Decompress a strict bytestring compressed using the LZW algorithm as described
in the PDF specifications.

It may return errors on invalid codes.
-}
decompress
  :: BS.ByteString -- ^ A strict bytestring of at least 2 bytes
  -> Either UnifiedError BS.ByteString
     -- ^ The uncompressed bytestring or an error
decompress stream = do
  let (output, state) = runState
        (whileM (moreToDecode $ BS.length stream * 8) (processCode stream))
        (initialStep . fromIntegral . BS.head $ stream)
  case psError state of
    Just anError -> Left anError
    Nothing      -> Right (BS.concat output)
