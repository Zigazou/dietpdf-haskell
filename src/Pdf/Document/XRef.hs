module Pdf.Document.XRef
  ( -- * XRef generation
    calcOffsets,
    xrefTable,
    xrefStreamWidth,
    xrefStreamTable,
    objectsNumberRange,
    PDFObjects,
    EncodedObject (..),
    EncodedObjects,
    ObjectOffsets,
  )
where

import Data.ByteString qualified as BS
import Data.Foldable (foldl', toList)
import Data.IntMap.Strict qualified as IM
import Data.Ix (range, rangeSize)
import Data.Logging (Logging)
import Data.Maybe (fromMaybe)
import Data.PDF.PDFObject
    ( PDFObject (PDFArray, PDFName, PDFNumber, PDFXRef, PDFXRefStream)
    , mkPDFArray
    )
import Data.PDF.PDFObjects (PDFObjects)
import Data.PDF.PDFWork (PDFWork, throwError)
import Data.PDF.XRefEntry (freeEntry, inUseEntry)
import Data.PDF.XRefSubsection (XRefSubsection (XRefSubsection))
import Data.Sequence (Seq ((:<|)))
import Data.Sequence qualified as SQ
import Data.UnifiedError (UnifiedError (XRefStreamNoW))

import Data.PDF.EncodedObject
    ( EncodedObject (EncodedObject, eoBinaryData, eoObjectLength, eoObjectNumber)
    )
import Data.PDF.EncodedObjects (EncodedObjects)
import Data.PDF.ObjectOffset
    ( ObjectOffset (DirectOffset, FreeEntry, InObjectStream)
    , getOffsetValue
    )
import Data.PDF.ObjectOffsets (ObjectOffsets, indexRange, insertFreeEntries)
import Pdf.Object.Object.ToPDFNumber (ToPDFNumber (mkPDFNumber))
import Pdf.Object.State (getValue)

import Util.Dictionary (Dictionary, mkDictionary)
import Util.Number (bytesNeededToEncode, encodeIntToBytes)

{- |
Given an encoded object, returns a list of tuples where the first element is the
object number and the second element is the index of the object in the object
stream.
-}
indexedObjects :: EncodedObject -> Seq (Int, ObjectOffset)
indexedObjects (EncodedObject number _len _bin embedded) =
  SQ.sort (go 0 embedded)
 where
  go _index SQ.Empty          = SQ.Empty
  go index (item :<| remains) = (item, InObjectStream item number index)
                              :<| go (index + 1) remains

{- |
Given an encoded object, calculates the offset of the object and the offsets of
the embedded objects.
-}
calcOffset :: Int -> EncodedObject -> (Int, ObjectOffsets)
calcOffset startOffset object =
  ( startOffset + eoObjectLength object
  , IM.insert number (DirectOffset number startOffset)
    $ IM.fromAscList (toList $ indexedObjects object)
  )
 where
  number = eoObjectNumber object

-- | Given a collection of encoded objects, calculates their offsets
calcOffsets :: Int -> EncodedObjects -> ObjectOffsets
calcOffsets startOffset = snd . go . fmap snd . IM.toAscList
  where
    go :: [EncodedObject] -> (Int, ObjectOffsets)
    go = foldl'
          (\(curOffset, curOffsets) object ->
            case calcOffset curOffset object of
              (newOffset, newOffsets) -> ( newOffset
                                         , IM.union curOffsets newOffsets
                                         )
          )
          (startOffset, IM.empty)

-- |
-- Given the objects offsets, return the range of numbers of the objects.
objectsNumberRange :: ObjectOffsets -> (Int, Int)
objectsNumberRange offsets =
  case (IM.minViewWithKey offsets, IM.maxViewWithKey offsets) of
    (Just ((minKey, _), _), Just ((maxKey, _), _)) -> (minKey, maxKey)
    _anyOtherCase                                  -> (0, 0)

-- |
-- Given a collection of encoded objects, generates an old format XRef table
xrefTable ::
  -- | Absolute offset of the first object
  Int ->
  -- | The encoded objects to reference
  EncodedObjects ->
  -- | The old format XRef table
  PDFObject
xrefTable startOffset objects
  | objects == mempty = PDFXRef []
  | otherwise = PDFXRef [xrefSubsection]
  where
    offsets = calcOffsets startOffset objects
    numRange = objectsNumberRange offsets
    entries =
      [ case IM.lookup index offsets of
          Just (DirectOffset _objectNumber offset) -> inUseEntry offset 0
          _anythingElse                            -> freeEntry
        | index <- range numRange
      ]
    xrefSubsection = XRefSubsection (fst numRange) (rangeSize numRange) entries

xrefStreamWidth :: (Logging m) => PDFObject -> PDFWork m Int
xrefStreamWidth object@PDFXRefStream {} = do
  w <- getValue "W" object
  case w of
    Just w' -> case getWidth w' of
      Just width     -> return width
      _anyOtherValue -> throwError (XRefStreamNoW "Cannot decode entries")
    Nothing -> throwError (XRefStreamNoW "No W field")
  where
    getWidth :: PDFObject -> Maybe Int
    getWidth (PDFArray array) = do
      typeWidth <- array SQ.!? 0
      offsetWidth <- array SQ.!? 1
      numberWidth <- array SQ.!? 2
      case (typeWidth, offsetWidth, numberWidth) of
        (PDFNumber typeWidth', PDFNumber offsetWidth', PDFNumber numberWidth') ->
          return $ round (typeWidth' + offsetWidth' + numberWidth')
        _anyOtherCase -> Nothing
    getWidth _anyOtherCase = Nothing
xrefStreamWidth _anyOtherCase = throwError (XRefStreamNoW "Not a PDFXRefStream")

mkBinary :: [(Int, Int)] -> BS.ByteString
mkBinary = BS.concat . fmap (uncurry encodeIntToBytes)

{- | Create an entry for the cross-reference stream.

There are three types of entries in a cross-reference stream:

- Type 0 entries define the linked list of free objects (corresponding to f
  entries in a cross-reference table).
  - column 0: The type of this entry, which must be 0.
  - column 1: The object number of the next free object.
  - column 2: The generation number to use if this object number is used again.
- Type 1 entries define objects that are in use but are not compressed
  (corresponding to n entries in a cross-reference table).
  - column 0: The type of this entry, which must be 1.
  - column 1: The byte offset of the object, starting from the beginning of the
    file.
  - column 2: The generation number of the object. Default value: 0.
- Type 2 entries define compressed objects.
  - column 0: The type of this entry, which must be 2.
  - column 1: The object number of the object stream in which this object is
    stored. (The generation number of the object stream is implicitly 0.)
  - column 2: The index of this object within the object stream.
-}
mkEntry :: Int -> Int -> Int -> ObjectOffset -> BS.ByteString
mkEntry _number offsetWidth numberWidth FreeEntry{} =
  mkBinary [(1, 0), (offsetWidth, 0), (numberWidth, 0)]
mkEntry _number offsetWidth numberWidth (DirectOffset _ offset) =
  mkBinary [(1, 1), (offsetWidth, offset), (numberWidth, 0)]
mkEntry _number offsetWidth numberWidth (InObjectStream _ objStream index) =
  mkBinary [(1, 2), (offsetWidth, objStream), (numberWidth, index)]

{- |
Given a collection of encoded objects, generates a cross-reference stream.
-}
xrefStreamTable
  :: Int -- ^ Number of the object to create
  -> Int -- ^ Absolute offset of the first object
  -> EncodedObjects -- ^ The encoded objects to reference
  -> PDFObject -- ^ The new format XRef table
xrefStreamTable number startOffset objects
  | objects == mempty = PDFXRef []
  | otherwise = PDFXRefStream number 0 xrefDictionary mkStream
  where
    -- The offsets of the objects.
    offsets :: ObjectOffsets
    offsets = insertFreeEntries (calcOffsets startOffset objects)

    -- The first and last object numbers (including objects in object streams)
    firstNumber, lastNumber :: Int
    (firstNumber, lastNumber) = indexRange offsets

    -- The number of bytes needed to encode the object number.
    numberWidth :: Int
    numberWidth = bytesNeededToEncode lastNumber

    -- The number of bytes needed to encode the offset.
    offsetWidth :: Int
    offsetWidth = bytesNeededToEncode (getOffsetValue lastNumberOffset)

    -- The offset of the last object.
    lastNumberOffset :: ObjectOffset
    lastNumberOffset = fromMaybe (DirectOffset 0 startOffset)
                                 (IM.lookup lastNumber offsets)

    -- The number of bytes needed to encode the offset.
    -- The dictionary of the cross-reference stream.
    xrefDictionary :: Dictionary PDFObject
    xrefDictionary =
      mkDictionary
        [ ("Type", PDFName "XRef"),
          ( "W",
            mkPDFArray
              [ mkPDFNumber (1 :: Double),
                mkPDFNumber offsetWidth,
                mkPDFNumber numberWidth
              ]
          ),
          ("Index", mkPDFArray [ mkPDFNumber firstNumber
                               , mkPDFNumber (IM.size offsets)
                               ]
          ),
          ("Size", mkPDFNumber (lastNumber + 1))
        ]

    -- The stream of the cross-reference stream.
    mkStream :: BS.ByteString
    mkStream =
      IM.foldrWithKey
        (\index offset stream -> BS.append
          (mkEntry index offsetWidth numberWidth offset)
          stream
        )
        ""
        offsets
