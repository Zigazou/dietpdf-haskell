{-|
Generate PDF cross-reference tables and streams.

Provides utilities for creating both old-format XRef tables and modern XRef
streams, including offset calculation, cross-reference entry generation, and
handling of compressed object streams.
-}
module PDF.Document.XRef
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

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (foldl', toList)
import Data.IntMap.Strict qualified as IM
import Data.Ix (range, rangeSize)
import Data.Logging (Logging)
import Data.Maybe (fromMaybe)
import Data.PDF.EncodedObject
    ( EncodedObject (EncodedObject, eoBinaryData, eoObjectLength, eoObjectNumber)
    )
import Data.PDF.EncodedObjects (EncodedObjects)
import Data.PDF.ObjectOffset
    ( ObjectOffset (DirectOffset, FreeEntry, InObjectStream)
    , getOffsetValue
    )
import Data.PDF.ObjectOffsets (ObjectOffsets, indexRange, insertFreeEntries)
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

import PDF.Object.Object.ToPDFNumber (ToPDFNumber (mkPDFNumber))
import PDF.Object.State (getValue)

import Util.Dictionary (Dictionary, mkDictionary)
import Util.Number (bytesNeededToEncode, encodeIntToBytes)

{-|
Extract indexed objects from an encoded object.

Returns a sorted sequence of tuples containing object numbers and their
cross-reference offsets within the object stream. Used to track objects embedded
in a single object stream.
-}
indexedObjects :: EncodedObject -> Seq (Int, ObjectOffset)
indexedObjects (EncodedObject number _len _bin embedded) =
  SQ.sort (go 0 embedded)
 where
  go _index SQ.Empty          = SQ.Empty
  go index (item :<| remains) = (item, InObjectStream item number index)
                              :<| go (index + 1) remains

{-|
Calculate the offset of an encoded object and its embedded objects.

Given a starting byte offset, computes the next offset and generates
cross-reference entries for both the direct object and any embedded objects
within it.
-}
calcOffset :: Int -> EncodedObject -> (Int, ObjectOffsets)
calcOffset startOffset object =
  ( startOffset + eoObjectLength object
  , IM.insert number (DirectOffset number startOffset)
    $ IM.fromAscList (toList $ indexedObjects object)
  )
 where
  number = eoObjectNumber object

{-|
Calculate offsets for all encoded objects in a collection.

Iterates through encoded objects starting from the given offset and builds a
complete map of object numbers to their cross-reference offsets.
-}
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

{-|
Find the minimum and maximum object numbers in a cross-reference table.

Returns a tuple of (minNumber, maxNumber) spanning all objects. Returns (0, 0)
if the offset table is empty.
-}
objectsNumberRange :: ObjectOffsets -> (Int, Int)
objectsNumberRange offsets =
  case (IM.minViewWithKey offsets, IM.maxViewWithKey offsets) of
    (Just ((minKey, _), _), Just ((maxKey, _), _)) -> (minKey, maxKey)
    _anyOtherCase                                  -> (0, 0)

{-|
Generate an old-format PDF cross-reference table.

Creates a traditional XRef table with subsections mapping object numbers to byte
offsets or free object chains. Used for PDF files without stream compression
support.

@Int@: Absolute byte offset of the first object @EncodedObjects@: Collection of
encoded objects to reference
-}
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

{-|
Extract the width field from an XRef stream object.

Reads the W (width) dictionary entry which specifies the byte widths for type,
offset, and object number fields in the cross-reference stream. Returns the sum
of all three widths, or fails if the W field is missing or malformed.
-}
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

{-|
Encode integer pairs to binary bytes.

Converts a list of (width, value) pairs into their binary representation,
concatenating the results. Used to build variable-width cross-reference stream
entries.
-}
mkBinary :: [(Int, Int)] -> ByteString
mkBinary = BS.concat . fmap (uncurry encodeIntToBytes)

{-|
Create a binary entry for a cross-reference stream.

Generates a variable-width entry based on the object's cross-reference type:

* Type 0 (free entry): links to next free object; records type, next object
  number, and generation number
* Type 1 (direct entry): in-use non-compressed object; records type, byte offset
  from file start, and generation number
* Type 2 (compressed entry): object within an object stream; records type,
  object stream number, and object index within stream
-}
mkEntry :: Int -> Int -> Int -> ObjectOffset -> ByteString
mkEntry _number offsetWidth numberWidth FreeEntry{} =
  mkBinary [(1, 0), (offsetWidth, 0), (numberWidth, 0)]
mkEntry _number offsetWidth numberWidth (DirectOffset _ offset) =
  mkBinary [(1, 1), (offsetWidth, offset), (numberWidth, 0)]
mkEntry _number offsetWidth numberWidth (InObjectStream _ objStream index) =
  mkBinary [(1, 2), (offsetWidth, objStream), (numberWidth, index)]

{-|
Generate a modern PDF XRef stream (PDF 1.5+).

Creates a new-format cross-reference stream containing entries for all objects
and the objects they contain (including compressed objects in object streams).
Incorporates free entry tracking and calculates appropriate field widths.
Returns a PDFXRefStream object ready to be embedded in the PDF.

- @Int@: Object number for the XRef stream itself
- @Int@: Absolute byte offset of the first object
- @EncodedObjects@: Collection of encoded objects to reference
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
    mkStream :: ByteString
    mkStream =
      IM.foldrWithKey
        (\index offset stream -> BS.append
          (mkEntry index offsetWidth numberWidth offset)
          stream
        )
        ""
        offsets
