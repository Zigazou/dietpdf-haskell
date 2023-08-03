module Pdf.Document.XRef
  ( -- * XRef generation
    calcOffsets
  , encodeObject
  , xrefTable
  , xrefStreamWidth
  , xrefStreamTable
  , objectsNumberRange
  , PDFObjects
  , EncodedObject(..)
  , EncodedObjects
  , ObjectOffsets
  ) where

import           Data.Foldable                  ( foldl' )
import qualified Data.IntMap.Strict            as IM
import           Data.Ix                        ( range
                                                , rangeSize
                                                )
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFXRef
                                                  , PDFXRefStream
                                                  , PDFName
                                                  , PDFArray
                                                  , PDFNumber
                                                  )
                                                , XRefSubsection(XRefSubsection)
                                                , freeEntry
                                                , inUseEntry
                                                , mkPDFArray
                                                , ToPDFNumber(mkPDFNumber)
                                                )
import           Pdf.Document.Collection        ( PDFObjects
                                                , EncodedObjects
                                                , ObjectOffsets
                                                , EncodedObject
                                                  ( EncodedObject
                                                  , eoBinaryData
                                                  )
                                                , encodeObject
                                                )
import           Util.Number                    ( encodeIntToBytes
                                                , bytesNeededToEncode
                                                )
import           Util.Dictionary                ( mkDictionary )
import qualified Data.ByteString               as BS
import           Data.Maybe                     ( fromMaybe )
import           Util.UnifiedError              ( FallibleT
                                                , UnifiedError(XRefStreamNoW)
                                                )
import qualified Data.Sequence                 as SQ
import           Pdf.Object.State               ( getValue )
import           Control.Monad.Trans.Except     ( throwE )
import           Util.Logging                   ( Logging )

-- | Given a collection of encoded objects, calculates their offsets
calcOffsets :: Int -> EncodedObjects -> ObjectOffsets
calcOffsets startOffset = snd . calcOffset . fmap snd . IM.toAscList
 where
  calcOffset :: [EncodedObject] -> (Int, ObjectOffsets)
  calcOffset = foldl'
    (\(offset, offsets) (EncodedObject number objectLength _) ->
      (offset + objectLength, IM.insert number offset offsets)
    )
    (startOffset, IM.empty)

{- |
Given the objects offsets, return the range of numbers of the objects.
-}
objectsNumberRange :: ObjectOffsets -> (Int, Int)
objectsNumberRange offsets =
  case (IM.minViewWithKey offsets, IM.maxViewWithKey offsets) of
    (Just ((minKey, _), _), Just ((maxKey, _), _)) -> (minKey, maxKey)
    _anyOtherCase -> (0, 0)

{- |
Given a collection of encoded objects, generates an old format XRef table
-}
xrefTable
  :: Int -- ^ Absolute offset of the first object
  -> EncodedObjects -- ^ The encoded objects to reference
  -> PDFObject -- ^ The old format XRef table
xrefTable startOffset objects | objects == mempty = PDFXRef []
                              | otherwise         = PDFXRef [xrefSubsection]
 where
  offsets  = calcOffsets startOffset objects
  numRange = objectsNumberRange offsets
  entries =
    [ maybe freeEntry (`inUseEntry` 0) (IM.lookup index offsets)
    | index <- range numRange
    ]
  xrefSubsection = XRefSubsection (fst numRange) (rangeSize numRange) entries

xrefStreamWidth :: Logging m => PDFObject -> FallibleT m Int
xrefStreamWidth object@PDFXRefStream{} = do
  w <- getValue "W" object
  case w of
    Just w' -> case getWidth w' of
      Just width     -> return width
      _anyOtherValue -> throwE (XRefStreamNoW "Cannot decode entries")
    Nothing -> throwE (XRefStreamNoW "No W field")
 where
  getWidth :: PDFObject -> Maybe Int
  getWidth (PDFArray array) = do
    typeWidth   <- array SQ.!? 0
    offsetWidth <- array SQ.!? 1
    numberWidth <- array SQ.!? 2
    case (typeWidth, offsetWidth, numberWidth) of
      (PDFNumber typeWidth', PDFNumber offsetWidth', PDFNumber numberWidth') ->
        return $ round (typeWidth' + offsetWidth' + numberWidth')
      _anyOtherCase -> Nothing
  getWidth _anyOtherCase = Nothing
xrefStreamWidth _anyOtherCase = throwE (XRefStreamNoW "Not a PDFXRefStream")

-- | Given a collection of encoded objects, generates a cross-reference stream.
xrefStreamTable
  :: Int -- ^ Number of the object to create
  -> Int -- ^ Absolute offset of the first object
  -> EncodedObjects -- ^ The encoded objects to reference
  -> PDFObject -- ^ The new format XRef table
xrefStreamTable number startOffset objects
  | objects == mempty = PDFXRef []
  | otherwise         = PDFXRefStream number 0 xrefDictionary stream

 where
  offsets                   = calcOffsets startOffset objects
  (firstNumber, lastNumber) = objectsNumberRange offsets
  numberByteCount           = bytesNeededToEncode lastNumber
  lastNumberOffset = fromMaybe startOffset (IM.lookup lastNumber offsets)
  offsetByteCount           = bytesNeededToEncode lastNumberOffset
  offsetCount               = IM.size objects

  xrefDictionary            = mkDictionary
    [ ("Type", PDFName "XRef")
    , ( "W"
      , mkPDFArray
        [ mkPDFNumber (1 :: Double)
        , mkPDFNumber offsetByteCount
        , mkPDFNumber numberByteCount
        ]
      )
    , ("Index", mkPDFArray [mkPDFNumber firstNumber, mkPDFNumber offsetCount])
    , ("Size" , mkPDFNumber (BS.length stream))
    ]

  stream = IM.foldrWithKey
    (\index offset stream' -> BS.concat
      [ "\x01"
      , encodeIntToBytes offsetByteCount offset
      , encodeIntToBytes numberByteCount index
      , stream'
      ]
    )
    ""
    offsets
