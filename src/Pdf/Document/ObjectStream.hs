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
module Pdf.Document.ObjectStream
  ( explodeObjects
  , explodeDocument
  , insert
  , isObjectStreamable
  ) where

import           Control.Applicative            ( (<|>) )
import           Data.Binary.Parser             ( Get
                                                , isDigit
                                                , many'
                                                , parseOnly
                                                , skipWhile
                                                , takeWhile1
                                                )
import qualified Data.ByteString               as BS
import           Util.Number                    ( fromInt )

import           Pdf.Document.Document          ( PDFDocument
                                                , cFilter
                                                )
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFIndirectObject
                                                  , PDFName
                                                  , PDFNumber
                                                  , PDFObjectStream
                                                  , PDFIndirectObjectWithGraphics
                                                  , PDFIndirectObjectWithStream
                                                  )
                                                , fromPDFObject
                                                , isWhiteSpace
                                                , mkPDFNumber
                                                )
import           Pdf.Object.State               ( getStream
                                                , getValue
                                                )
import           Control.Monad                  ( forM )
import           Data.Functor                   ( (<&>) )
import           Pdf.Object.Unfilter            ( unfilter )
import           Pdf.Object.Parser.Container    ( arrayP
                                                , dictionaryP
                                                )
import           Pdf.Object.Parser.HexString    ( hexStringP )
import           Pdf.Object.Parser.Keyword      ( keywordP )
import           Pdf.Object.Parser.Name         ( nameP )
import           Pdf.Object.Parser.Number       ( numberP )
import           Pdf.Object.Parser.Reference    ( referenceP )
import           Pdf.Object.Parser.String       ( stringP )
import           Util.Ascii                     ( asciiDIGITZERO )
import           Util.UnifiedError              ( UnifiedError
                                                  ( NoObjectToEncode
                                                  , ParseError
                                                  , ObjectStreamNotFound
                                                  )
                                                , FallibleT
                                                )
import           Data.Foldable                  ( foldl' )
import           Util.Dictionary                ( Dictionary
                                                , mkDictionary
                                                )
import           Util.Logging                   ( Logging )
import           Control.Monad.Trans.Except     ( throwE )
import           Pdf.Document.Collection        ( PDFObjects )
import           Data.IntMap                    ( union
                                                , singleton
                                                , fromList
                                                )
import qualified Pdf.Document.Document         as D

data ObjectStream = ObjectStream
  { osCount   :: !Int
  , osOffset  :: !Int
  , osIndices :: !BS.ByteString
  , osObjects :: !BS.ByteString
  }

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
  toInt bs = BS.foldl'
    (\num digit -> num * 10 + fromIntegral (digit - asciiDIGITZERO))
    0
    bs

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
explodeDocument :: Logging m => PDFDocument -> FallibleT m PDFDocument
explodeDocument = (<&> D.fromList) . explodeList . D.toList
 where
  extractList :: Logging m => ObjectStream -> FallibleT m [PDFObject]
  extractList (ObjectStream _ _ indices objects) = do
    numOffsets <- parseObjectNumberOffsets indices
    exploded   <- forM numOffsets $ \(NumberOffset objectNumber offset) -> do
      case parseOnly oneObjectP (BS.drop offset objects) of
        Left  msg    -> throwE $! ParseError ("", fromIntegral offset, msg)
        Right object -> return $! PDFIndirectObject objectNumber 0 object
    return $! exploded

  explodeList :: Logging m => [PDFObject] -> FallibleT m [PDFObject]
  explodeList (objstm@PDFObjectStream{} : xs) = do
    extracted <- getObjectStream objstm >>= extractList
    remains   <- explodeList xs
    return (extracted ++ remains)
  explodeList (object : xs) = do
    remains <- explodeList xs
    return (object : remains)
  explodeList [] = return []

getObjectStream :: Logging m => PDFObject -> FallibleT m ObjectStream
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
    _anyOtherValue -> throwE ObjectStreamNotFound

{- |
Look for every object stream and extract the objects from these object streams.

Any other object is kept as is.
-}
explodeObjects :: Logging m => PDFObjects -> FallibleT m PDFObjects
explodeObjects objects = mapM explodeObject objects <&> foldr union mempty
 where
  extractObjects :: Logging m => ObjectStream -> FallibleT m PDFObjects
  extractObjects (ObjectStream _ _ indices objects') = do
    numOffsets <- parseObjectNumberOffsets indices
    exploded   <- forM numOffsets $ \(NumberOffset objectNumber offset) -> do
      case parseOnly oneObjectP (BS.drop offset objects') of
        Left msg -> throwE $! ParseError ("", fromIntegral offset, msg)
        Right object ->
          return (objectNumber, PDFIndirectObject objectNumber 0 object)
    return $! fromList exploded

  explodeObject :: Logging m => PDFObject -> FallibleT m PDFObjects
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
insert
  :: Logging m
  => PDFDocument -- ^ A `CollectionOf` `PDFObject` to embed in the object stream
  -> Int -- ^ The number of the resulting `PDFObjectStream`
  -> FallibleT m PDFObject
insert objects num | objects == mempty = throwE NoObjectToEncode
                   | osCount objStm == 0 = throwE NoObjectToEncode
                   | otherwise = return $ PDFObjectStream num 0 dict stream
 where
  objStm = insertObjects (cFilter isObjectStreamable objects)
  dict :: Dictionary PDFObject
  dict = mkDictionary
    [ ("Type" , PDFName "ObjStm")
    , ("N"    , mkPDFNumber (osCount objStm))
    , ("First", mkPDFNumber (osOffset objStm))
    ]
  stream = dropLastByte . BS.concat $ [osIndices objStm, osObjects objStm]
