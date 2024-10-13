{- |
This module handles object streams.

An object stream, is a stream object in which a sequence of indirect objects
may be stored, as an alternative to their being stored at the outermost file
level.

Object streams are first introduced in PDF 1.5.

The purpose of object streams is to allow indirect objects other than streams
to be stored more compactly by using the facilities provided by stream
compression filters.

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

type ObjectStream :: Type
data ObjectStream = ObjectStream
  { osCount   :: !Int
  , osOffset  :: !Int
  , osIndices :: !BS.ByteString
  , osObjects :: !BS.ByteString
  }

type NumberOffset :: Type
data NumberOffset = NumberOffset !Int !Int

emptyObjectStream :: ObjectStream
emptyObjectStream =
  ObjectStream { osCount = 0, osOffset = 0, osIndices = "", osObjects = "" }

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

oneObjectP :: Get PDFObject
oneObjectP = do
  skipWhile isWhiteSpace
  item <- itemP
  skipWhile isWhiteSpace
  return $! item

integerP :: Get Int
integerP = takeWhile1 isDigit <&> toInt
 where
  toInt :: BS.ByteString -> Int
  toInt = BS.foldl'
    (\num digit -> num * 10 + fromIntegral (digit - asciiDIGITZERO))
    0

objectNumberOffsetP :: Get NumberOffset
objectNumberOffsetP = do
  skipWhile isWhiteSpace
  objectNumber <- integerP
  skipWhile isWhiteSpace
  offset <- integerP
  skipWhile isWhiteSpace
  return $! NumberOffset objectNumber offset

parseObjectNumberOffsets
  :: Logging m => BS.ByteString -> FallibleT m [NumberOffset]
parseObjectNumberOffsets indices =
  case parseOnly (many' objectNumberOffsetP) indices of
    Left  err    -> throwE (ParseError ("", 0, err))
    Right result -> return $! result

{- |
Look for every object stream and extract the objects from these object streams.

Any other object is kept as is.
-}
explodeDocument :: Logging m => PDFDocument -> PDFWork m PDFDocument
explodeDocument = (<&> D.fromList) . explodeList . D.toList

extractList :: Logging m => ObjectStream -> FallibleT m [PDFObject]
extractList (ObjectStream _ _ indices objects) = do
  numOffsets <- parseObjectNumberOffsets indices
  exploded   <- forM numOffsets $ \(NumberOffset objectNumber offset) -> do
    case parseOnly oneObjectP (BS.drop offset objects) of
      Left  msg    -> throwE $! ParseError ("", fromIntegral offset, msg)
      Right object -> return $! PDFIndirectObject objectNumber 0 object
  return $! exploded

explodeList :: Logging m => [PDFObject] -> PDFWork m [PDFObject]
explodeList (objstm@PDFObjectStream{} : xs) = do
  extracted <- getObjectStream objstm >>= lift . extractList
  remains   <- explodeList xs
  return (extracted ++ remains)
explodeList (object : xs) = do
  remains <- explodeList xs
  return (object : remains)
explodeList [] = return []

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

{- |
Look for every object stream and extract the objects from these object streams.

Any other object is kept as is.
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

{- |
Tells if a `PDFObject` may be embedded in an object stream.
-}
isObjectStreamable :: PDFObject -> Bool
isObjectStreamable PDFIndirectObject{} = True
isObjectStreamable _anyOtherValue      = False

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

insertObjects :: PDFDocument -> ObjectStream
insertObjects = foldl' appendObject emptyObjectStream

dropLastByte :: BS.ByteString -> BS.ByteString
dropLastByte str = BS.take (BS.length str - 1) str

{- |
Create an object stream from a list of `PDFObject`.

Object which are not streamable are simply ignored.

The object stream is uncompressed. It can be compressed later.
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
