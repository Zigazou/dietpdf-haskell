{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}

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
  ( extract
  , explode
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
import qualified Data.Map.Strict               as Map
import           Util.Number                    ( fromInt )

import           Pdf.Document.Document          ( PDFDocument
                                                , dFilter
                                                , fromList
                                                , singleton
                                                )
import           Pdf.Object.Object              ( Dictionary
                                                , PDFObject
                                                  ( PDFIndirectObject
                                                  , PDFName
                                                  , PDFNumber
                                                  , PDFObjectStream
                                                  )
                                                , fromPDFObject
                                                , isWhiteSpace
                                                )
import           Pdf.Object.State               ( getStream
                                                , getValue
                                                , queryE
                                                )
import           Control.Monad                  ( forM )
import           Data.Functor                   ( (<&>) )
import           Pdf.Object.Unfilter            ( unfilter )
import           Pdf.Parser.Container           ( arrayP
                                                , dictionaryP
                                                )
import           Pdf.Parser.HexString           ( hexStringP )
import           Pdf.Parser.Keyword             ( keywordP )
import           Pdf.Parser.Name                ( nameP )
import           Pdf.Parser.Number              ( numberP )
import           Pdf.Parser.Reference           ( referenceP )
import           Pdf.Parser.String              ( stringP )
import           Util.Ascii                     ( asciiDIGITZERO )
import           Util.Errors                    ( UnifiedError
                                                  ( NoObjectToEncode
                                                  , ParseError
                                                  )
                                                )
import           Data.Foldable                  ( foldl' )

data ObjectStream = ObjectStream
  { osCount   :: Int
  , osOffset  :: Int
  , osIndices :: BS.ByteString
  , osObjects :: BS.ByteString
  }

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

integerP :: Get Int
integerP = takeWhile1 isDigit <&> toInt
 where
  toInt :: BS.ByteString -> Int
  toInt bs = BS.foldl'
    (\num digit -> num * 10 + fromIntegral (digit - asciiDIGITZERO))
    0
    bs

objectNumberOffsetP :: Get (Int, Int)
objectNumberOffsetP = do
  skipWhile isWhiteSpace
  objectNumber <- integerP
  skipWhile isWhiteSpace
  offset <- integerP
  skipWhile isWhiteSpace
  return (objectNumber, offset)

parseObjectNumberOffsets :: BS.ByteString -> Either UnifiedError [(Int, Int)]
parseObjectNumberOffsets indices =
  case parseOnly (many' objectNumberOffsetP) indices of
    Left  err    -> Left (ParseError ("", 0, err))
    Right result -> Right result

extractObjects :: ObjectStream -> Either UnifiedError PDFDocument
extractObjects (ObjectStream _ _ indices objects) = do
  numOffsets <- parseObjectNumberOffsets indices
  exploded   <- forM numOffsets $ \(objectNumber, offset) -> do
    case parseOnly itemP (BS.drop offset objects) of
      Left  msg    -> Left $ ParseError ("", fromIntegral offset, msg)
      Right object -> return $ PDFIndirectObject objectNumber 0 object
  return $ fromList exploded

getObjectStream :: PDFObject -> Either UnifiedError (Maybe ObjectStream)
getObjectStream object@PDFObjectStream{} = queryE object $ do
  objectN      <- getValue "N"
  objectOffset <- getValue "First"

  case (objectN, objectOffset) of
    (Just (PDFNumber n), Just (PDFNumber offset)) -> do
      unfilteredStream <- unfilter >> getStream
      let (indices, objects) = BS.splitAt (floor offset) unfilteredStream
      return $ Just ObjectStream { osCount   = floor n
                                 , osOffset  = floor offset
                                 , osIndices = indices
                                 , osObjects = objects
                                 }

    _anyOtherValue -> return Nothing
getObjectStream _ = return Nothing

{- |
Extract objects contained in an object stream from a `PDFObject`.

If the `PDFObject` is not an object stream (Type = /ObjStm) it returns an
empty list.

If the `PDFObject` contains no object stream, an empty list is returned.
-}
extract
  :: PDFObject -- ^ A `PDFIndirectObject` of type /ObjStm with a stream
  -> Either UnifiedError PDFDocument
extract object = getObjectStream object >>= \case
  Nothing     -> return mempty
  Just objStm -> extractObjects objStm

explode :: PDFDocument -> PDFDocument
explode = foldr explode' mempty
 where
  explode' :: PDFObject -> PDFDocument -> PDFDocument
  explode' objstm@PDFObjectStream{} result = case extract objstm of
    Left  _       -> result <> singleton objstm
    Right objects -> result <> objects
  explode' object result = result <> singleton object

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
  :: PDFDocument -- ^ A `CollectionOf` `PDFObject` to embed in the object stream
  -> Int -- ^ The number of the resulting `PDFObjectStream`
  -> Either UnifiedError PDFObject
insert objects num | objects == mempty = Left NoObjectToEncode
                   | osCount objStm == 0 = Left NoObjectToEncode
                   | otherwise = return $ PDFObjectStream num 0 dict stream
 where
  objStm = insertObjects (dFilter isObjectStreamable objects)
  dict :: Dictionary
  dict = Map.fromList
    [ ("Type" , PDFName "ObjStm")
    , ("N"    , PDFNumber . fromIntegral . osCount $ objStm)
    , ("First", PDFNumber . fromIntegral . osOffset $ objStm)
    ]
  stream = dropLastByte . BS.concat $ [osIndices objStm, osObjects objStm]
