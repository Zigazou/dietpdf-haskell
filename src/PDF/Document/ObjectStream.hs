{-|
This module handles object streams.

An object stream, is a stream object in which a sequence of indirect objects may
be stored, as an alternative to their being stored at the outermost file level.

Object streams are first introduced in PDF 1.5.

The purpose of object streams is to allow indirect objects other than streams to
be stored more compactly by using the facilities provided by stream compression
filters.

The term “compressed object” is used regardless of whether the stream is
actually encoded with a compression filter.
-}
module PDF.Document.ObjectStream
  ( explodeObjects
  , explodeDocument
  , makeObjectStreamFromObjects
  , isObjectStreamable
  , explodeList
  ) where

import Control.Applicative ((<|>))
import Control.Monad (forM)
import Control.Monad.State (lift)
import Control.Monad.Trans.Except (throwE)

import Data.Binary.Parser
    ( Get
    , isDigit
    , many'
    , parseOnly
    , skipWhile
    , takeWhile1
    )
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Fallible (FallibleT)
import Data.Foldable (foldl')
import Data.Functor ((<&>))
import Data.IntMap (fromList, singleton, union)
import Data.Kind (Type)
import Data.Logging (Logging)
import Data.PDF.PDFDocument (PDFDocument, cFilter)
import Data.PDF.PDFDocument qualified as D
import Data.PDF.PDFWork (PDFWork, throwError)
import Data.UnifiedError
    ( UnifiedError (NoObjectToEncode, ObjectStreamNotFound, ParseError)
    )

import PDF.Document.PDFObjects (PDFObjects)
import PDF.Object.Object
    ( PDFObject (PDFIndirectObject, PDFIndirectObjectWithGraphics, PDFIndirectObjectWithStream, PDFName, PDFNumber, PDFObjectStream)
    , fromPDFObject
    , isWhiteSpace
    , mkPDFNumber
    )
import PDF.Object.Parser.Container (arrayP, dictionaryP)
import PDF.Object.Parser.HexString (hexStringP)
import PDF.Object.Parser.Keyword (keywordP)
import PDF.Object.Parser.Name (nameP)
import PDF.Object.Parser.Number (numberP)
import PDF.Object.Parser.Reference (referenceP)
import PDF.Object.Parser.String (stringP)
import PDF.Object.State (getStream, getValue)
import PDF.Processing.Unfilter (unfilter)

import Util.Ascii (asciiDIGITZERO)
import Util.Dictionary (Dictionary, mkDictionary)
import Util.Number (fromInt)

{-|
Representation of a PDF object stream.

An object stream stores compressed indirect objects with their offsets and
counts. The stream is split into two regions: number/offset index pairs and the
actual object data.

* @osCount@: Number of objects in the stream
* @osOffset@: Byte offset where object data starts (end of index section)
* @osIndices@: Binary index containing object numbers and offsets
* @osObjects@: Binary data containing the actual objects
-}
type ObjectStream :: Type
data ObjectStream = ObjectStream
  { osCount   :: !Int
  , osOffset  :: !Int
  , osIndices :: !ByteString
  , osObjects :: !ByteString
  }

{-|
Object number and byte offset pair from an object stream index.

Stores the object number and its byte offset within the object stream's object
data section.
-}
type NumberOffset :: Type
data NumberOffset = NumberOffset !Int !Int

{-|
Construct an empty object stream.

Initializes an object stream with zero count and empty index/object sections.
-}
emptyObjectStream :: ObjectStream
emptyObjectStream =
  ObjectStream { osCount = 0, osOffset = 0, osIndices = "", osObjects = "" }

{-|
Parse a single PDF object from binary data.

Matches any valid PDF object type: names, strings, references, numbers,
keywords, hex strings, arrays, or dictionaries.
-}
itemP :: Get PDFObject
itemP =
  nameP
    <|> stringP
    <|> referenceP
    <|> numberP
    <|> keywordP
    <|> hexStringP
    <|> arrayP
    <|> dictionaryP

{-|
Parse a single PDF object, skipping surrounding whitespace.

Reads one complete PDF object from the input stream and strips leading and
trailing whitespace.
-}
oneObjectP :: Get PDFObject
oneObjectP = do
  skipWhile isWhiteSpace
  item <- itemP
  skipWhile isWhiteSpace
  return $! item

{-|
Parse a decimal integer from ASCII digits.

Reads one or more digit characters and converts them to an integer value.
-}
integerP :: Get Int
integerP = takeWhile1 isDigit <&> toInt
 where
  toInt :: ByteString -> Int
  toInt = BS.foldl'
    (\num digit -> num * 10 + fromIntegral (digit - asciiDIGITZERO))
    0

{-|
Parse an object number and byte offset pair from the object stream index.

Reads whitespace-separated object number and byte offset values that form the
index of an object stream.
-}
objectNumberOffsetP :: Get NumberOffset
objectNumberOffsetP = do
  skipWhile isWhiteSpace
  objectNumber <- integerP
  skipWhile isWhiteSpace
  offset <- integerP
  skipWhile isWhiteSpace
  return $! NumberOffset objectNumber offset

{-|
Parse all object number and offset pairs from an index section.

Decodes the entire object stream index into a list of 'NumberOffset' pairs,
returning parse errors if the index format is invalid.
-}
parseObjectNumberOffsets
  :: Logging m => ByteString -> FallibleT m [NumberOffset]
parseObjectNumberOffsets indices =
  case parseOnly (many' objectNumberOffsetP) indices of
    Left  err    -> throwE (ParseError ("", 0, err))
    Right result -> return $! result

{-|
Extract all objects from object streams in a PDF document.

Searches the document for object stream objects and extracts their embedded
indirect objects. Other objects are kept unchanged. Returns a new document with
flattened object structure.
-}
explodeDocument :: Logging m => PDFDocument -> PDFWork m PDFDocument
explodeDocument = (<&> D.fromList) . explodeList . D.toList

{-|
Extract all indirect objects from an object stream.

Parses the object stream's index and object data sections, reconstructing
indirect objects with their original numbers from the stream.
-}
extractList :: Logging m => ObjectStream -> FallibleT m [PDFObject]
extractList (ObjectStream _ _ indices objects) = do
  numOffsets <- parseObjectNumberOffsets indices
  exploded   <- forM numOffsets $ \(NumberOffset objectNumber offset) -> do
    case parseOnly oneObjectP (BS.drop offset objects) of
      Left  msg    -> throwE $! ParseError ("", fromIntegral offset, msg)
      Right object -> return $! PDFIndirectObject objectNumber 0 object
  return $! exploded

{-|
Recursively extract objects from object streams in a list.

Processes a list of PDF objects, extracting contents of object stream objects
and leaving other objects unchanged. Returns a flattened list of all objects.
-}
explodeList :: Logging m => [PDFObject] -> PDFWork m [PDFObject]
explodeList (objstm@PDFObjectStream{} : xs) = do
  extracted <- getObjectStream objstm >>= lift . extractList
  remains   <- explodeList xs
  return (extracted ++ remains)
explodeList (object : xs) = do
  remains <- explodeList xs
  return (object : remains)
explodeList [] = return []

{-|
Extract and parse the object stream metadata from a PDF object.

Reads the N (object count) and First (index size) dictionary entries, unfilters
the stream, and partitions it into index and object data sections. Fails if the
object is not a valid object stream.
-}
getObjectStream :: Logging m => PDFObject -> PDFWork m ObjectStream
getObjectStream object = do
  objectN      <- getValue "N" object
  objectOffset <- getValue "First" object

  case (objectN, objectOffset) of
    (Just (PDFNumber n), Just (PDFNumber offset)) -> do
      unfilteredStream <- unfilter object >>= getStream
      let (indices, objects) = BS.splitAt (floor offset) unfilteredStream
      return $ ObjectStream { osCount   = floor n
                            , osOffset  = floor offset
                            , osIndices = indices
                            , osObjects = objects
                            }
    _anyOtherValue -> throwError ObjectStreamNotFound

{-|
Extract all objects from object streams in a collection.

Converts a collection of PDF objects into a map, extracting any embedded objects
from object streams and flattening the structure. Returns a unified IntMap keyed
by object number.
-}
explodeObjects :: Logging m => PDFObjects -> PDFWork m PDFObjects
explodeObjects objects = mapM explodeObject objects <&> foldr union mempty
 where
  extractObjects :: Logging m => ObjectStream -> PDFWork m PDFObjects
  extractObjects (ObjectStream _ _ indices objects') = do
    numOffsets <- lift (parseObjectNumberOffsets indices)
    exploded   <- forM numOffsets $ \(NumberOffset objectNumber offset) -> do
      case parseOnly oneObjectP (BS.drop offset objects') of
        Left msg -> throwError $! ParseError ("", fromIntegral offset, msg)
        Right object ->
          return (objectNumber, PDFIndirectObject objectNumber 0 object)
    return $! fromList exploded

  explodeObject :: Logging m => PDFObject -> PDFWork m PDFObjects
  explodeObject objstm@PDFObjectStream{} =
    getObjectStream objstm >>= extractObjects
  explodeObject object@(PDFIndirectObject number _ _) =
    return $ singleton number object
  explodeObject object@(PDFIndirectObjectWithGraphics number _ _ _) =
    return $ singleton number object
  explodeObject object@(PDFIndirectObjectWithStream number _ _ _) =
    return $ singleton number object
  explodeObject object = return $ singleton 0 object

{-|
Test whether a PDF object can be embedded in an object stream.

Returns 'True' for indirect objects, 'False' for all other types. Only
'PDFIndirectObject' types are streamable according to PDF specification.
-}
isObjectStreamable :: PDFObject -> Bool
isObjectStreamable PDFIndirectObject{} = True
isObjectStreamable _anyOtherValue      = False

{-|
Append an indirect object to an object stream.

Adds an object to the stream by appending its serialized form to the object data
section and updating the index with its object number and byte offset.
Non-indirect objects are ignored.
-}
appendObject :: ObjectStream -> PDFObject -> ObjectStream
appendObject objStm (PDFIndirectObject num _ object) = ObjectStream
  { osCount   = osCount objStm + 1
  , osOffset  = BS.length newIndices
  , osIndices = newIndices
  , osObjects = newObjects
  }
 where
  newIndices = BS.concat
    [ osIndices objStm
    , fromInt num
    , " "
    , fromInt . BS.length . osObjects $ objStm
    , " "
    ]
  newObjects = BS.concat [osObjects objStm, fromPDFObject object, " "]
appendObject objStm _ = objStm

{-|
Build an object stream from all streamable objects in a document.

Iterates through a PDF document, appending all streamable indirect objects to
create a single object stream. Non-streamable objects are skipped.
-}
insertObjects :: PDFDocument -> ObjectStream
insertObjects = foldl' appendObject emptyObjectStream

{-|
Remove the trailing byte from a byte string.

Returns all bytes except the last one. Used to remove trailing space from object
stream data.
-}
dropLastByte :: ByteString -> ByteString
dropLastByte str = BS.take (BS.length str - 1) str

{-|
Create a PDF object stream from a collection of objects.

Filters the document to include only streamable indirect objects, embeds them
into a new object stream with the given number, and returns the stream object.
The stream is uncompressed; compression can be applied later. Fails if no
streamable objects are present.

@PDFDocument@: Collection of objects to embed (non-streamable objects ignored)
@Int@: Object number for the resulting object stream
-}
makeObjectStreamFromObjects
  :: Logging m
  => PDFDocument -- ^ A `CollectionOf` `PDFObject` to embed in the object stream
  -> Int -- ^ The number of the resulting `PDFObjectStream`
  -> PDFWork m PDFObject
makeObjectStreamFromObjects objects num
  | objects == mempty   = throwError NoObjectToEncode
  | osCount objStm == 0 = throwError NoObjectToEncode
  | otherwise           = return $ PDFObjectStream num 0 dict stream
 where
  objStm = insertObjects (cFilter isObjectStreamable objects)
  dict :: Dictionary PDFObject
  dict = mkDictionary
    [ ("Type" , PDFName "ObjStm")
    , ("N"    , mkPDFNumber (osCount objStm))
    , ("First", mkPDFNumber (osOffset objStm))
    ]
  stream = dropLastByte . BS.concat $ [osIndices objStm, osObjects objStm]
