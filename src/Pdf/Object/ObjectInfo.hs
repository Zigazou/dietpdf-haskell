module Pdf.Object.ObjectInfo
  ( objectInfo
  ) where

import Data.ByteString qualified as BS
import Data.Fallible (FallibleT)
import Data.Logging (Logging)
import Data.Map qualified as Map
import Data.ObjectCategory (ObjectCategory (Other))
import Data.ObjectInfo
    ( ObjectInfo (ObjectInfo, oCategory, oDescription, oNumber, oOffset, oStream)
    , StreamInfo (StreamInfo, sFilteredSize, sUnfilteredSize)
    )
import Data.Text.Lazy (toStrict)

import Formatting (format, int, (%))
import Formatting.ByteStringFormatter (utf8)

import Pdf.Object.Object.PDFObject
    ( PDFObject (PDFArray, PDFBool, PDFComment, PDFDictionary, PDFEndOfFile, PDFHexString, PDFIndirectObject, PDFIndirectObjectWithGraphics, PDFIndirectObjectWithStream, PDFKeyword, PDFName, PDFNull, PDFNumber, PDFObjectStream, PDFReference, PDFStartXRef, PDFString, PDFTrailer, PDFVersion, PDFXRef, PDFXRefStream)
    )
import Pdf.Object.ObjectCategory (objectCategory)
import Pdf.Object.State (getStream)
import Pdf.Object.Unfilter (unfilter)

import Util.Number (fromNumber)

objectInfo :: Logging IO => PDFObject -> Maybe Int -> FallibleT IO ObjectInfo
objectInfo (PDFComment comment) offset = return ObjectInfo
  { oNumber      = Nothing
  , oDescription = toStrict $ format ("{- " % utf8 % " -}") comment
  , oCategory    = Other
  , oStream      = Nothing
  , oOffset      = offset
  }

objectInfo (PDFVersion version) offset = return ObjectInfo
  { oNumber      = Nothing
  , oDescription = toStrict $ format ("VERSION=" % utf8) version
  , oCategory    = Other
  , oStream      = Nothing
  , oOffset      = offset
  }

objectInfo PDFEndOfFile offset = return ObjectInfo
  { oNumber      = Nothing
  , oDescription = "END-OF-FILE"
  , oCategory    = Other
  , oStream      = Nothing
  , oOffset      = offset
  }

objectInfo (PDFNumber number) offset = return ObjectInfo
  { oNumber = Nothing
  , oDescription = toStrict $ format ("number=" % utf8) (fromNumber number)
  , oCategory = Other
  , oStream = Nothing
  , oOffset = offset
  }

objectInfo (PDFKeyword keyword) offset = return ObjectInfo
  { oNumber      = Nothing
  , oDescription = toStrict $ format ("keyword=" % utf8) keyword
  , oCategory    = Other
  , oStream      = Nothing
  , oOffset      = offset
  }

objectInfo (PDFName name) offset = return ObjectInfo
  { oNumber      = Nothing
  , oDescription = toStrict $ format ("/" % utf8) name
  , oCategory    = Other
  , oStream      = Nothing
  , oOffset      = offset
  }

objectInfo (PDFString bytes) offset = return ObjectInfo
  { oNumber      = Nothing
  , oDescription = toStrict $ format ("string=" % utf8) bytes
  , oCategory    = Other
  , oStream      = Nothing
  , oOffset      = offset
  }

objectInfo (PDFHexString hexstring) offset = return ObjectInfo
  { oNumber      = Nothing
  , oDescription = toStrict $ format ("hexstring=" % utf8) hexstring
  , oCategory    = Other
  , oStream      = Nothing
  , oOffset      = offset
  }

objectInfo (PDFReference number revision) offset = return ObjectInfo
  { oNumber      = Just number
  , oDescription = toStrict $ format ("ref=" % int % " " % int) number revision
  , oCategory    = Other
  , oStream      = Nothing
  , oOffset      = offset
  }

objectInfo (PDFArray objects) offset = return ObjectInfo
  { oNumber      = Nothing
  , oDescription = toStrict $ format ("array[" % int % "]") (length objects)
  , oCategory    = Other
  , oStream      = Nothing
  , oOffset      = offset
  }

objectInfo (PDFDictionary dictionary) offset = return ObjectInfo
  { oNumber      = Nothing
  , oDescription = toStrict $ format ("dict[" % int % "]") (Map.size dictionary)
  , oCategory    = Other
  , oStream      = Nothing
  , oOffset      = offset
  }

objectInfo (PDFIndirectObject number _revision _object) offset = return ObjectInfo
  { oNumber      = Just number
  , oDescription = "indirect object"
  , oCategory    = Other
  , oStream      = Nothing
  , oOffset      = offset
  }

objectInfo object@(PDFIndirectObjectWithStream number _revision _dict stream) offset = do
  unfiltered <- unfilter object >>= getStream
  category <- objectCategory object

  return ObjectInfo
    { oNumber      = Just number
    , oDescription = "object with stream"
    , oCategory    = category
    , oStream      = Just StreamInfo
      { sFilteredSize   = BS.length stream
      , sUnfilteredSize = BS.length unfiltered
      }
    , oOffset      = offset
    }

objectInfo (PDFIndirectObjectWithGraphics number _revision _dict _objects) offset = return ObjectInfo
    { oNumber      = Just number
    , oDescription = "object with graphics"
    , oCategory    = Other
    , oStream      = Nothing
    , oOffset      = offset
    }

objectInfo object@(PDFObjectStream number _revision _object stream) offset = do
  unfiltered <- unfilter object >>= getStream
  category <- objectCategory object

  return ObjectInfo
    { oNumber      = Just number
    , oDescription = "object stream"
    , oCategory    = category
    , oStream      = Just StreamInfo
      { sFilteredSize   = BS.length stream
      , sUnfilteredSize = BS.length unfiltered
      }
    , oOffset      = offset
    }

objectInfo object@(PDFXRefStream number _revision _object stream) offset = do
  unfiltered <- unfilter object >>= getStream
  category <- objectCategory object

  return ObjectInfo
    { oNumber      = Just number
    , oDescription = "xref stream"
    , oCategory    = category
    , oStream      = Just StreamInfo
      { sFilteredSize   = BS.length stream
      , sUnfilteredSize = BS.length unfiltered
      }
    , oOffset      = offset
    }

objectInfo (PDFBool True ) offset = return ObjectInfo
  { oNumber      = Nothing
  , oDescription = "bool=true"
  , oCategory    = Other
  , oStream      = Nothing
  , oOffset      = offset
  }

objectInfo (PDFBool False) offset = return ObjectInfo
  { oNumber      = Nothing
  , oDescription = "bool=false"
  , oCategory    = Other
  , oStream      = Nothing
  , oOffset      = offset
  }

objectInfo PDFNull offset = return ObjectInfo
  { oNumber      = Nothing
  , oDescription = "null"
  , oCategory    = Other
  , oStream      = Nothing
  , oOffset      = offset
  }

objectInfo (PDFXRef _xref) offset = return ObjectInfo
  { oNumber      = Nothing
  , oDescription = "xref table"
  , oCategory    = Other
  , oStream      = Nothing
  , oOffset      = offset
  }

objectInfo (PDFTrailer (PDFDictionary _dict)) offset = return ObjectInfo
  { oNumber      = Nothing
  , oDescription = "trailer"
  , oCategory    = Other
  , oStream      = Nothing
  , oOffset      = offset
  }

objectInfo (PDFTrailer _) offset = return ObjectInfo
  { oNumber      = Nothing
  , oDescription = "invalid trailer"
  , oCategory    = Other
  , oStream      = Nothing
  , oOffset      = offset
  }

objectInfo (PDFStartXRef startOffset) offset = return ObjectInfo
  { oNumber      = Nothing
  , oDescription = toStrict $ format ("startxref=" % int % "]") startOffset
  , oCategory    = Other
  , oStream      = Nothing
  , oOffset      = offset
  }
