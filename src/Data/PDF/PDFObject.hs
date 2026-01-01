{-|
This module defines what is a PDF object and functions in relation with the
PDF specification.
-}
module Data.PDF.PDFObject
  ( -- * PDF object
    PDFObject
    ( PDFComment
    , PDFVersion
    , PDFEndOfFile
    , PDFNumber
    , PDFKeyword
    , PDFName
    , PDFString
    , PDFHexString
    , PDFReference
    , PDFArray
    , PDFDictionary
    , PDFIndirectObject
    , PDFIndirectObjectWithStream
    , PDFIndirectObjectWithGraphics
    , PDFObjectStream
    , PDFXRefStream
    , PDFBool
    , PDFNull
    , PDFXRef
    , PDFTrailer
    , PDFStartXRef
    )
  , mkEmptyPDFArray
  , mkEmptyPDFDictionary
  , mkPDFArray
  , mkPDFDictionary
  , hasDictionary
  , hasStream
  , isIndirect
  , getObjectNumber
  , isHeader
  , isTrailer
  , isReference
  ) where

import Data.Array (Array, mkArray, mkEmptyArray)
import Data.ByteString (ByteString)
import Data.Context (Context (Context), Contextual (ctx), ctx2)
import Data.Kind (Type)
import Data.PDF.GFXObjects (GFXObjects)
import Data.PDF.XRefSubsection (XRefSubsection)

import Util.Dictionary (Dictionary, mkDictionary, mkEmptyDictionary)

{-|
A PDF is a collection of objects, here named PDF objects.

Values contained are decoded, meaning they no longer contain escape sequences.
-}
type PDFObject :: Type
data PDFObject
  = -- | A comment (without the starting %)
    PDFComment !ByteString
  | -- | Version of the PDF (a special comment)
    PDFVersion !ByteString
  | -- | End of file (a special comment)
    PDFEndOfFile
  | -- | A number (always stored as a double)
    PDFNumber !Double
  | -- | A keyword
    PDFKeyword !ByteString
  | -- | A name (starting with /)
    PDFName !ByteString
  | -- | A string (unescaped and without parenthesis)
    PDFString !ByteString
  | -- | An hexadeicmal string (without less-than/greater-than signs)
    PDFHexString !ByteString
  | -- | A reference, number and generation (two integers followed by an `R`)
    PDFReference !Int !Int
  | -- | An array containing a list of objects
    PDFArray !(Array PDFObject)
  | -- | A dictionary containing key-value pairs
    PDFDictionary !(Dictionary PDFObject)
  | -- | An indirect object, object number, generation, object itself
    PDFIndirectObject !Int !Int !PDFObject
  | -- | An indirect object with a `ByteString` stream
    PDFIndirectObjectWithStream !Int !Int !(Dictionary PDFObject) !ByteString
  | -- | An indirect object with an `Array` of `GFXObject`
    PDFIndirectObjectWithGraphics !Int !Int !(Dictionary PDFObject) !GFXObjects
  | -- | An object stream, object number, generation, dictionary and stream
    PDFObjectStream !Int !Int !(Dictionary PDFObject) !ByteString
  | -- | An XRef stream, object number, generation, dictionary and stream
    PDFXRefStream !Int !Int !(Dictionary PDFObject) !ByteString
  | -- | A boolean (true or false)
    PDFBool !Bool
  | -- | A null value
    PDFNull
  | -- | An XRef table
    PDFXRef ![XRefSubsection]
  | -- | A trailer
    PDFTrailer !PDFObject
  | -- | A reference to an XRef table (offset from beginning of a PDF)
    PDFStartXRef !Int
  deriving stock (Show)

{-|
Create an empty `PDFDictionary`.
-}
mkEmptyPDFDictionary :: PDFObject
mkEmptyPDFDictionary = PDFDictionary mkEmptyDictionary

{-|
Create an empty `PDFArray`.
-}
mkEmptyPDFArray :: PDFObject
mkEmptyPDFArray = PDFArray mkEmptyArray

{-|
Create a `PDFDictionary` from a list of couples (key, value).
-}
mkPDFDictionary :: [(ByteString, PDFObject)] -> PDFObject
mkPDFDictionary = PDFDictionary . mkDictionary

{-|
Create a `PDFArray` from a list of `PDFObject`.
-}
mkPDFArray :: [PDFObject] -> PDFObject
mkPDFArray = PDFArray . mkArray

instance Eq PDFObject where
  (==) :: PDFObject -> PDFObject -> Bool
  (PDFComment x)       == (PDFComment y)       = x == y
  (PDFVersion _)       == (PDFVersion _)       = True
  PDFEndOfFile         == PDFEndOfFile         = True
  (PDFNumber    x    ) == (PDFNumber    y    ) = x == y
  (PDFKeyword   x    ) == (PDFKeyword   y    ) = x == y
  (PDFName      x    ) == (PDFName      y    ) = x == y
  (PDFString    x    ) == (PDFString    y    ) = x == y
  (PDFHexString x    ) == (PDFHexString y    ) = x == y
  (PDFReference xn xr) == (PDFReference yn yr) = xn == yn && xr == yr
  (PDFArray      x   ) == (PDFArray      y   ) = x == y
  (PDFDictionary x   ) == (PDFDictionary y   ) = x == y
  (PDFIndirectObject xn xr _) == (PDFIndirectObject yn yr _) =
    xn == yn && xr == yr
  (PDFIndirectObjectWithStream xn xr _ _) == (PDFIndirectObjectWithStream yn yr _ _)
    = xn == yn && xr == yr
  (PDFObjectStream xn xr _ _) == (PDFObjectStream yn yr _ _) =
    xn == yn && xr == yr
  (PDFXRefStream xn xr _ _) == (PDFXRefStream yn yr _ _) = xn == yn && xr == yr
  (PDFBool x              ) == (PDFBool y              ) = x == y
  PDFNull                   == PDFNull                   = True
  (PDFXRef      x)          == (PDFXRef      y)          = x == y
  (PDFTrailer   x)          == (PDFTrailer   y)          = x == y
  (PDFStartXRef x)          == (PDFStartXRef y)          = x == y
  _anyObjectA               == _anyObjectB               = False

objectRank :: PDFObject -> Int
objectRank (PDFVersion _)                  = 0
objectRank (PDFComment _)                  = 1
objectRank PDFNull                         = 2
objectRank (PDFBool      _  )              = 3
objectRank (PDFNumber    _  )              = 4
objectRank (PDFKeyword   _  )              = 5
objectRank (PDFName      _  )              = 6
objectRank (PDFString    _  )              = 7
objectRank (PDFHexString _  )              = 8
objectRank (PDFReference _ _)              = 9
objectRank (PDFArray      _ )              = 10
objectRank (PDFDictionary _ )              = 11
objectRank PDFIndirectObject{}             = 12
objectRank PDFIndirectObjectWithStream{}   = 13
objectRank PDFIndirectObjectWithGraphics{} = 14
objectRank PDFObjectStream{}               = 15
objectRank PDFXRefStream{}                 = 16
objectRank (PDFXRef      _)                = 17
objectRank (PDFTrailer   _)                = 18
objectRank (PDFStartXRef _)                = 19
objectRank PDFEndOfFile                    = 20

instance Ord PDFObject where
  compare :: PDFObject -> PDFObject -> Ordering
  compare (PDFComment x)   (PDFComment y)   = compare x y
  compare (PDFVersion _)   (PDFVersion _)   = EQ
  compare PDFEndOfFile     PDFEndOfFile     = EQ
  compare (PDFNumber    x) (PDFNumber    y) = compare x y
  compare (PDFKeyword   x) (PDFKeyword   y) = compare x y
  compare (PDFName      x) (PDFName      y) = compare x y
  compare (PDFString    x) (PDFString    y) = compare x y
  compare (PDFHexString x) (PDFHexString y) = compare x y
  compare (PDFReference xn xr) (PDFReference yn yr) =
    compare xn yn <> compare xr yr
  compare (PDFArray      x) (PDFArray      y) = compare x y
  compare (PDFDictionary x) (PDFDictionary y) = compare x y
  compare (PDFIndirectObject xn xr _) (PDFIndirectObject yn yr _) =
    compare xn yn <> compare xr yr
  compare (PDFIndirectObjectWithStream xn xr _ _) (PDFIndirectObjectWithStream yn yr _ _)
    = compare xn yn <> compare xr yr
  compare (PDFObjectStream xn xr _ _) (PDFObjectStream yn yr _ _) =
    compare xn yn <> compare xr yr
  compare (PDFXRefStream xn xr _ _) (PDFXRefStream yn yr _ _) =
    compare xn yn <> compare xr yr
  compare (PDFBool x)      (PDFBool y)      = compare x y
  compare PDFNull          PDFNull          = EQ
  compare (PDFXRef      x) (PDFXRef      y) = compare x y
  compare (PDFTrailer   x) (PDFTrailer   y) = compare x y
  compare (PDFStartXRef x) (PDFStartXRef y) = compare x y
  compare objectA objectB = compare (objectRank objectA) (objectRank objectB)

instance Contextual PDFObject where
  ctx :: PDFObject -> Context
  ctx (PDFComment _)                              = Context "com"
  ctx (PDFVersion _)                              = Context "ver"
  ctx PDFNull                                     = Context "null"
  ctx (PDFBool True)                              = Context "true"
  ctx (PDFBool False)                             = Context "false"
  ctx (PDFNumber value)                           = ctx value
  ctx (PDFKeyword name)                           = ctx2 '*' name
  ctx (PDFName name)                              = ctx2 '/' name
  ctx (PDFString _)                               = Context "str"
  ctx (PDFHexString _)                            = Context "hex"
  ctx (PDFReference num ver)                      = ctx2 '@' (num, ver)
  ctx (PDFArray _)                                = Context "array"
  ctx (PDFDictionary _)                           = Context "dict"
  ctx (PDFIndirectObject num ver _)               = ctx (num, ver)
  ctx (PDFIndirectObjectWithStream num ver _ _)   = ctx (num, ver)
  ctx (PDFIndirectObjectWithGraphics num ver _ _) = ctx (num, ver)
  ctx (PDFObjectStream num ver _ _)               = ctx (num, ver)
  ctx (PDFXRefStream num ver _ _)                 = ctx (num, ver)
  ctx (PDFXRef      _)                            = Context "xref"
  ctx (PDFTrailer   _)                            = Context "trail"
  ctx (PDFStartXRef _)                            = Context "startxref"
  ctx PDFEndOfFile                                = Context "eof"

{-|
Determine if a `PDFObject` has a dictionary.
-}
hasDictionary :: PDFObject -> Bool
hasDictionary (PDFIndirectObject _ _ (PDFDictionary _)) = True
hasDictionary PDFIndirectObjectWithStream{}             = True
hasDictionary PDFObjectStream{}                         = True
hasDictionary PDFXRefStream{}                           = True
hasDictionary PDFDictionary{}                           = True
hasDictionary (PDFTrailer (PDFDictionary _))            = True
hasDictionary _anyOtherObject                           = False

{-|
Determine if a `PDFObject` has a stream.
-}
hasStream :: PDFObject -> Bool
hasStream PDFIndirectObjectWithStream{} = True
hasStream PDFObjectStream{}             = True
hasStream PDFXRefStream{}               = True
hasStream _anyOtherObject               = False

isIndirect :: PDFObject -> Bool
isIndirect PDFIndirectObject{}             = True
isIndirect PDFIndirectObjectWithStream{}   = True
isIndirect PDFIndirectObjectWithGraphics{} = True
isIndirect PDFObjectStream{}               = True
isIndirect PDFXRefStream{}                 = True
isIndirect _anyOtherObject                 = False

getObjectNumber :: PDFObject -> Maybe Int
getObjectNumber (PDFIndirectObject number _ _)               = Just number
getObjectNumber (PDFIndirectObjectWithStream number _ _ _)   = Just number
getObjectNumber (PDFIndirectObjectWithGraphics number _ _ _) = Just number
getObjectNumber (PDFObjectStream number _ _ _)               = Just number
getObjectNumber (PDFXRefStream number _ _ _)                 = Just number
getObjectNumber _anyOtherObject                              = Nothing

isHeader :: PDFObject -> Bool
isHeader PDFVersion{}    = True
isHeader _anyOtherObject = False

isTrailer :: PDFObject -> Bool
isTrailer PDFTrailer{}    = True
isTrailer PDFXRefStream{} = True
isTrailer _anyOtherObject = False

isReference :: PDFObject -> Bool
isReference PDFReference{}  = True
isReference _anyOtherObject = False
