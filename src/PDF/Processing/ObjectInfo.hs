module PDF.Processing.ObjectInfo
  ( objectInfo
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IntMap qualified as IM
import Data.Logging (Logging)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.PDF.ObjectCategory (ObjectCategory (Other))
import Data.PDF.ObjectInfo
    ( ObjectInfo (ObjectInfo, oCategory, oDescription, oEmbedded, oNumber, oOffset, oStream)
    , StreamInfo (StreamInfo, sFilteredSize, sUnfilteredSize)
    )
import Data.PDF.PDFObject
    ( PDFObject (PDFArray, PDFBool, PDFComment, PDFDictionary, PDFEndOfFile, PDFHexString, PDFIndirectObject, PDFIndirectObjectWithGraphics, PDFIndirectObjectWithStream, PDFKeyword, PDFName, PDFNull, PDFNumber, PDFObjectStream, PDFReference, PDFStartXRef, PDFString, PDFTrailer, PDFVersion, PDFXRef, PDFXRefStream)
    )
import Data.PDF.PDFWork (PDFWork)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Text.Lazy (toStrict)

import Formatting (format, int, (%))
import Formatting.ByteStringFormatter (utf8)

import PDF.Document.ObjectStream (explodeObjects)
import PDF.Object.State (getStream)
import PDF.Processing.ObjectCategory (objectCategory)
import PDF.Processing.Unfilter (unfilter)

import Util.Number (fromNumber)

getTypeSubType :: PDFObject -> Maybe Text
getTypeSubType object = getTypeSubType' object >>= Just . decodeUtf8Lenient

getTypeSubType' :: PDFObject -> Maybe ByteString
getTypeSubType' (PDFDictionary dict) =
  case (Map.lookup "Type" dict, Map.lookup "Subtype" dict) of
  (Just (PDFName objectType), Just (PDFName objectSubType)) ->
    Just (objectType <> "/" <> objectSubType)
  (Just (PDFName objectType), Nothing) -> Just objectType
  (Nothing, Just (PDFName objectSubType)) -> Just objectSubType
  _anyOtherCase -> Nothing
getTypeSubType' (PDFIndirectObject _major _minor object) = getTypeSubType' object
getTypeSubType' (PDFIndirectObjectWithStream _major _minor object _stream) =
  getTypeSubType' (PDFDictionary object)
getTypeSubType' (PDFIndirectObjectWithGraphics _major _minor object _stream) =
  getTypeSubType' (PDFDictionary object)
getTypeSubType' _anyOtherObject = Nothing

getObjectType :: PDFObject -> Text
getObjectType (PDFComment comment) =
  toStrict $ format ("{- " % utf8 % " -}") comment
getObjectType (PDFVersion version) =
  toStrict $ format ("VERSION=" % utf8) version
getObjectType PDFEndOfFile = "END-OF-FILE"
getObjectType (PDFNumber number) =
  toStrict $ format ("number=" % utf8) (fromNumber number)
getObjectType (PDFKeyword keyword) =
  toStrict $ format ("keyword=" % utf8) keyword
getObjectType (PDFName name) = toStrict $ format ("/" % utf8) name
getObjectType (PDFString bytes) = toStrict $ format ("string=" % utf8) bytes
getObjectType (PDFHexString hexstring) =
  toStrict $ format ("hexstring=" % utf8) hexstring
getObjectType (PDFReference number revision) =
  toStrict $ format ("ref=" % int % " " % int) number revision
getObjectType PDFArray{} = "array"
getObjectType PDFDictionary{} = "dictionary"
getObjectType PDFIndirectObject{} = "indirect object"
getObjectType PDFIndirectObjectWithStream{} = "object with stream"
getObjectType PDFIndirectObjectWithGraphics{} = "object with graphics"
getObjectType PDFObjectStream{} = "object stream"
getObjectType PDFXRefStream{} = "object stream"
getObjectType (PDFBool True ) = "true"
getObjectType (PDFBool False) = "false"
getObjectType PDFNull = "null"
getObjectType (PDFXRef _xref) = "xref table"
getObjectType (PDFTrailer (PDFDictionary _dict)) = "trailer"
getObjectType (PDFTrailer _) = "invalid trailer"
getObjectType (PDFStartXRef _startOffset) = "startxref"

objectInfo :: Logging IO => PDFObject -> Maybe Int -> PDFWork IO ObjectInfo
objectInfo (PDFComment comment) offset = return ObjectInfo
  { oNumber      = Nothing
  , oDescription = toStrict $ format ("{- " % utf8 % " -}") comment
  , oCategory    = Other
  , oStream      = Nothing
  , oOffset      = offset
  , oEmbedded    = mempty
  }

objectInfo (PDFVersion version) offset = return ObjectInfo
  { oNumber      = Nothing
  , oDescription = toStrict $ format ("VERSION=" % utf8) version
  , oCategory    = Other
  , oStream      = Nothing
  , oOffset      = offset
  , oEmbedded    = mempty
  }

objectInfo PDFEndOfFile offset = return ObjectInfo
  { oNumber      = Nothing
  , oDescription = "END-OF-FILE"
  , oCategory    = Other
  , oStream      = Nothing
  , oOffset      = offset
  , oEmbedded    = mempty
  }

objectInfo (PDFNumber number) offset = return ObjectInfo
  { oNumber      = Nothing
  , oDescription = toStrict $ format ("number=" % utf8) (fromNumber number)
  , oCategory    = Other
  , oStream      = Nothing
  , oOffset      = offset
  , oEmbedded    = mempty
  }

objectInfo (PDFKeyword keyword) offset = return ObjectInfo
  { oNumber      = Nothing
  , oDescription = toStrict $ format ("keyword=" % utf8) keyword
  , oCategory    = Other
  , oStream      = Nothing
  , oOffset      = offset
  , oEmbedded    = mempty
  }

objectInfo (PDFName name) offset = return ObjectInfo
  { oNumber      = Nothing
  , oDescription = toStrict $ format ("/" % utf8) name
  , oCategory    = Other
  , oStream      = Nothing
  , oOffset      = offset
  , oEmbedded    = mempty
  }

objectInfo (PDFString bytes) offset = return ObjectInfo
  { oNumber      = Nothing
  , oDescription = toStrict $ format ("string=" % utf8) bytes
  , oCategory    = Other
  , oStream      = Nothing
  , oOffset      = offset
  , oEmbedded    = mempty
  }

objectInfo (PDFHexString hexstring) offset = return ObjectInfo
  { oNumber      = Nothing
  , oDescription = toStrict $ format ("hexstring=" % utf8) hexstring
  , oCategory    = Other
  , oStream      = Nothing
  , oOffset      = offset
  , oEmbedded    = mempty
  }

objectInfo (PDFReference number revision) offset = return ObjectInfo
  { oNumber      = Just number
  , oDescription = toStrict $ format ("ref=" % int % " " % int) number revision
  , oCategory    = Other
  , oStream      = Nothing
  , oOffset      = offset
  , oEmbedded    = mempty
  }

objectInfo (PDFArray objects) offset = return ObjectInfo
  { oNumber      = Nothing
  , oDescription = toStrict $ format ("array[" % int % "]") (length objects)
  , oCategory    = Other
  , oStream      = Nothing
  , oOffset      = offset
  , oEmbedded    = mempty
  }

objectInfo (PDFDictionary dictionary) offset = return ObjectInfo
  { oNumber      = Nothing
  , oDescription = toStrict $ format ("dict[" % int % "]") (Map.size dictionary)
  , oCategory    = Other
  , oStream      = Nothing
  , oOffset      = offset
  , oEmbedded    = mempty
  }

objectInfo (PDFIndirectObject number _revision object) offset = return ObjectInfo
  { oNumber      = Just number
  , oDescription =
      fromMaybe (T.concat ["indirect object ", getObjectType object])
                (getTypeSubType object)
  , oCategory    = Other
  , oStream      = Nothing
  , oOffset      = offset
  , oEmbedded    = mempty
  }

objectInfo object@(PDFIndirectObjectWithStream number _revision _dict stream) offset = do
  unfiltered <- unfilter object >>= getStream
  category <- objectCategory object

  return ObjectInfo
    { oNumber      = Just number
    , oDescription = fromMaybe "object with stream" (getTypeSubType object)
    , oCategory    = category
    , oStream      = Just StreamInfo
      { sFilteredSize   = BS.length stream
      , sUnfilteredSize = BS.length unfiltered
      }
    , oOffset      = offset
    , oEmbedded    = mempty
    }

objectInfo (PDFIndirectObjectWithGraphics number _revision dict _objects) offset = return ObjectInfo
  { oNumber      = Just number
  , oDescription = fromMaybe "object with graphics" (getTypeSubType (PDFDictionary dict))
  , oCategory    = Other
  , oStream      = Nothing
  , oOffset      = offset
  , oEmbedded    = mempty
  }

objectInfo object@(PDFObjectStream number _revision _object stream) offset = do
  unfiltered <- unfilter object >>= getStream
  category <- objectCategory object
  embedded <- explodeObjects (IM.singleton 0 object)
          >>= mapM (`objectInfo` Nothing)

  return ObjectInfo
    { oNumber      = Just number
    , oDescription = fromMaybe "object stream" (getTypeSubType object)
    , oCategory    = category
    , oStream      = Just StreamInfo
      { sFilteredSize   = BS.length stream
      , sUnfilteredSize = BS.length unfiltered
      }
    , oOffset      = offset
    , oEmbedded    = IM.elems embedded
    }

objectInfo object@(PDFXRefStream number _revision _object stream) offset = do
  unfiltered <- unfilter object >>= getStream
  category <- objectCategory object

  return ObjectInfo
    { oNumber      = Just number
    , oDescription = fromMaybe "xref stream" (getTypeSubType object)
    , oCategory    = category
    , oStream      = Just StreamInfo
      { sFilteredSize   = BS.length stream
      , sUnfilteredSize = BS.length unfiltered
      }
    , oOffset      = offset
    , oEmbedded    = mempty
    }

objectInfo (PDFBool True ) offset = return ObjectInfo
  { oNumber      = Nothing
  , oDescription = "bool=true"
  , oCategory    = Other
  , oStream      = Nothing
  , oOffset      = offset
  , oEmbedded    = mempty
  }

objectInfo (PDFBool False) offset = return ObjectInfo
  { oNumber      = Nothing
  , oDescription = "bool=false"
  , oCategory    = Other
  , oStream      = Nothing
  , oOffset      = offset
  , oEmbedded    = mempty
  }

objectInfo PDFNull offset = return ObjectInfo
  { oNumber      = Nothing
  , oDescription = "null"
  , oCategory    = Other
  , oStream      = Nothing
  , oOffset      = offset
  , oEmbedded    = mempty
  }

objectInfo (PDFXRef _xref) offset = return ObjectInfo
  { oNumber      = Nothing
  , oDescription = "xref table"
  , oCategory    = Other
  , oStream      = Nothing
  , oOffset      = offset
  , oEmbedded    = mempty
  }

objectInfo (PDFTrailer (PDFDictionary _dict)) offset = return ObjectInfo
  { oNumber      = Nothing
  , oDescription = "trailer"
  , oCategory    = Other
  , oStream      = Nothing
  , oOffset      = offset
  , oEmbedded    = mempty
  }

objectInfo (PDFTrailer _) offset = return ObjectInfo
  { oNumber      = Nothing
  , oDescription = "invalid trailer"
  , oCategory    = Other
  , oStream      = Nothing
  , oOffset      = offset
  , oEmbedded    = mempty
  }

objectInfo (PDFStartXRef startOffset) offset = return ObjectInfo
  { oNumber      = Nothing
  , oDescription = toStrict $ format ("startxref=" % int) startOffset
  , oCategory    = Other
  , oStream      = Nothing
  , oOffset      = offset
  , oEmbedded    = mempty
  }
