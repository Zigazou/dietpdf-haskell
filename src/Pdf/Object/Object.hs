{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE InstanceSigs #-}

{-|
This module defines what is a PDF object and functions in relation with the
PDF specification.
-}
module Pdf.Object.Object
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

    -- * Conversion
  , fromPDFObject

    -- * Getting info about a `PDFObject`
  , objectInfo
  , hasKey
  , hasDictionary
  , hasStream

    -- * PDF indirect object
  , updateStream

    -- * PDF characters
  , isDelimiter
  , isPlusMinus
  , isWhiteSpace
  , isKeywordCharacter
  , isOctal
  , isStringEscapeSequence
  , isStringRegularChar
  , isNameRegularChar
  , spaceIfNeeded

    -- * XRef
  , xrefCount
  , inUseEntry
  , freeEntry
  , XRefState(InUseEntry, FreeEntry)
  , XRefEntry(XRefEntry, xreOffset, xreGeneration, xreState)
  , XRefSubsection(XRefSubsection, xrssStart, xrssCount, xrssEntries)
  ) where

import qualified Data.ByteString               as BS
import qualified Data.ByteString.UTF8          as BSU
import qualified Data.Text.Lazy                as TL
import qualified Data.Text                     as TS
import           Data.Text.Encoding             ( encodeUtf8 )
import           Formatting                     ( format
                                                , (%)
                                                , int
                                                , text
                                                , left
                                                )
import           Formatting.ByteStringFormatter ( utf8 )
import qualified Data.Map.Strict               as Map
import           Data.Ix                        ( inRange )
import           Data.List                      ( foldl' )
import           Data.Foldable                  ( toList )
import           Data.Maybe                     ( isJust
                                                , isNothing
                                                )
import           Data.Word                      ( Word8 )
import           Util.Ascii                     ( asciiDIGITSEVEN
                                                , asciiDIGITZERO
                                                , asciiLOWERA
                                                , asciiLOWERZ
                                                , asciiNUL
                                                , asciiNUMBERSIGN
                                                , asciiUPPERA
                                                , asciiUPPERZ
                                                )
import           Util.Name                      ( fromName )
import           Util.Number                    ( fromInt
                                                , fromNumber
                                                )
import           Util.String                    ( fromHexString
                                                , fromString
                                                )
import           Util.Dictionary                ( Dictionary
                                                , mkEmptyDictionary
                                                , mkDictionary
                                                , dictHasKey
                                                )
import           Util.Array                     ( Array
                                                , mkEmptyArray
                                                , mkArray
                                                )
import           Pdf.Graphics.Object            ( GFXObject
                                                , separateGfx
                                                )

{-|
Test if a byte is a PDF delimiter.

The following characters are considered delimiters: `()<>[]{}/%`.
 -}
isDelimiter :: Word8 -> Bool
isDelimiter = isJust . flip BS.elemIndex "()<>[]{}/%"

-- | Test if a byte is a plus or minus sign.
isPlusMinus :: Word8 -> Bool
isPlusMinus = isJust . flip BS.elemIndex "-+"

{-|
Test if a byte is a white space.

The following characters are considered white spaces:

- `asciiNUL`
- `asciiTAB`
- `asciiLF`
- `asciiFF`
- `asciiCR`
- `asciiSPACE`
-}
isWhiteSpace :: Word8 -> Bool
isWhiteSpace = isJust . flip BS.elemIndex "\0\t\n\f\r "

{-|
Test if a byte is a keyword character.

Keyword characters are either lowercase or uppercase alphabetical characters.
-}
isKeywordCharacter :: Word8 -> Bool
isKeywordCharacter byte =
  inRange (asciiLOWERA, asciiLOWERZ) byte
    || inRange (asciiUPPERA, asciiUPPERZ) byte

{-|
Test if a byte a an octal digit.

The following characters are considered octal digits: 0, 1, 2, 3, 4, 5, 6 and 7.
-}
isOctal :: Word8 -> Bool
isOctal = inRange (asciiDIGITZERO, asciiDIGITSEVEN)

{-|
Test if a byte is a valid escaped character in a PDF string.

The following characters are considered valid: n, r, t, b, f, (, ) and \\.
-}
isStringEscapeSequence :: Word8 -> Bool
isStringEscapeSequence = isJust . flip BS.elemIndex "nrtbf()\\"

{-|
Test if a byte is a valid regular character in a PDF string.

Any character that is not (, ) or \\ is a valid regular character.
-}
isStringRegularChar :: Word8 -> Bool
isStringRegularChar = isNothing . flip BS.elemIndex "()\\"

{-|
Test if a byte is a valid character for a PDF name.

A PDF name may not contain an `asciiNUL` character, white space or delimiter.

The `asciiNUMBERSIGN` is reserved for escaping.
-}
isNameRegularChar :: Word8 -> Bool
isNameRegularChar byte | byte == asciiNUMBERSIGN = False
                       | byte == asciiNUL        = False
                       | isWhiteSpace byte       = False
                       | isDelimiter byte        = False
                       | otherwise               = True

-- | State of an entry in an XRef table (old format).
data XRefState
  = InUseEntry -- ^ Regular entry
  | FreeEntry -- ^ Free entry
  deriving stock (Eq, Show)

instance Ord XRefState where
  compare :: XRefState -> XRefState -> Ordering
  compare InUseEntry InUseEntry = EQ
  compare FreeEntry  FreeEntry  = EQ
  compare InUseEntry FreeEntry  = GT
  compare FreeEntry  InUseEntry = LT

-- | Entry in an XRef table (old format).
data XRefEntry = XRefEntry
  { xreOffset     :: !Int
    -- ^ Offset from the beginning of the PDF file
  , xreGeneration :: !Int
    -- ^ Generation number (usually 0)
  , xreState      :: !XRefState
    -- ^ State of the entry
  }
  deriving stock (Eq, Show)

instance Ord XRefEntry where
  compare :: XRefEntry -> XRefEntry -> Ordering
  compare (XRefEntry xo xg xs) (XRefEntry yo yg ys) =
    compare xo yo <> compare xg yg <> compare xs ys

-- | Create an XRef entry with the `InUseEntry` state.
inUseEntry :: Int -> Int -> XRefEntry
inUseEntry offset generation = XRefEntry offset generation InUseEntry

{-|
Create an XRef entry with the `FreeEntry` state.

A free entry state has an offset and a generation number of 0.
-}
freeEntry :: XRefEntry
freeEntry = XRefEntry 0 0 FreeEntry

{-|
An XRef table may contain many subsections.

An XRef subsection itself may contain many entries.
-}
data XRefSubsection = XRefSubsection
  { xrssStart   :: !Int
    -- ^ Index of the first entry
  , xrssCount   :: !Int
    -- ^ Entry count
  , xrssEntries :: ![XRefEntry]
    -- ^ Entries
  }
  deriving stock (Eq, Show)

instance Ord XRefSubsection where
  compare :: XRefSubsection -> XRefSubsection -> Ordering
  compare (XRefSubsection xs xc xe) (XRefSubsection ys yc ye) =
    compare xs ys <> compare xc yc <> compare xe ye

data XRefStmFieldType =
    XRSFFreeEntry
  | XRSFUncompressedObject
  | XRSFCompressedObject
  deriving stock (Eq, Show)

{-|
A PDF is a collection of objects, here named PDF objects.

Values contained are decoded, meaning they no longer contain escape sequences.
-}
data PDFObject
  = -- | A comment (without the starting %)
    PDFComment BS.ByteString
  | -- | Version of the PDF (a special comment)
    PDFVersion BS.ByteString
  | -- | End of file (a special comment)
    PDFEndOfFile
  | -- | A number (always stored as a double)
    PDFNumber Double
  | -- | A keyword
    PDFKeyword BS.ByteString
  | -- | A name (starting with /)
    PDFName BS.ByteString
  | -- | A string (unescaped and without parenthesis)
    PDFString BS.ByteString
  | -- | An hexadeicmal string (without less-than/greater-than signs)
    PDFHexString BS.ByteString
  | -- | A reference, number and generation (two integers followed by an `R`)
    PDFReference Int Int
  | -- | An array containing a list of objects
    PDFArray (Array PDFObject)
  | -- | A dictionary containing key-value pairs
    PDFDictionary (Dictionary PDFObject)
  | -- | An indirect object, object number, generation, object itself
    PDFIndirectObject Int Int PDFObject
  | -- | An indirect object with a `ByteString` stream
    PDFIndirectObjectWithStream Int Int (Dictionary PDFObject) BS.ByteString
  | -- | An indirect object with an `Array` of `GFXObject`
    PDFIndirectObjectWithGraphics Int Int (Dictionary PDFObject) (Array GFXObject)
  | -- | An object stream, object number, generation, dictionary and stream
    PDFObjectStream Int Int (Dictionary PDFObject) BS.ByteString
  | -- | A boolean (true or false)
    PDFBool Bool
  | -- | A null value
    PDFNull
  | -- | An XRef table
    PDFXRef [XRefSubsection]
  | -- | A trailer
    PDFTrailer PDFObject
  | -- | A reference to an XRef table (offset from beginning of a PDF)
    PDFStartXRef Int
  deriving stock (Show)

{- |
Create an empty `PDFDictionary`.
-}
mkEmptyPDFDictionary :: PDFObject
mkEmptyPDFDictionary = PDFDictionary mkEmptyDictionary

{- |
Create an empty `PDFArray`.
-}
mkEmptyPDFArray :: PDFObject
mkEmptyPDFArray = PDFArray mkEmptyArray

{- |
Create a `PDFDictionary` from a list of couples (key, value).
-}
mkPDFDictionary :: [(BS.ByteString, PDFObject)] -> PDFObject
mkPDFDictionary = PDFDictionary . mkDictionary

{- |
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
  (PDFBool x)      == (PDFBool y)      = x == y
  PDFNull          == PDFNull          = True
  (PDFXRef      x) == (PDFXRef      y) = x == y
  (PDFTrailer   x) == (PDFTrailer   y) = x == y
  (PDFStartXRef x) == (PDFStartXRef y) = x == y
  _anyObjectA      == _anyObjectB      = False

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
objectRank (PDFXRef      _)                = 16
objectRank (PDFTrailer   _)                = 17
objectRank (PDFStartXRef _)                = 18
objectRank PDFEndOfFile                    = 19

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
  compare (PDFBool x)      (PDFBool y)      = compare x y
  compare PDFNull          PDFNull          = EQ
  compare (PDFXRef      x) (PDFXRef      y) = compare x y
  compare (PDFTrailer   x) (PDFTrailer   y) = compare x y
  compare (PDFStartXRef x) (PDFStartXRef y) = compare x y
  compare objectA objectB = compare (objectRank objectA) (objectRank objectB)

{- |
Indicates whether the `PDFObject` ends with a delimiter when converted to a
`ByteString`.
-}
endsWithDelimiter :: PDFObject -> Bool
endsWithDelimiter PDFComment{}                    = True
endsWithDelimiter PDFVersion{}                    = True
endsWithDelimiter PDFEndOfFile                    = True
endsWithDelimiter PDFNumber{}                     = False
endsWithDelimiter PDFKeyword{}                    = False
endsWithDelimiter PDFName{}                       = False
endsWithDelimiter PDFString{}                     = True
endsWithDelimiter PDFHexString{}                  = True
endsWithDelimiter PDFReference{}                  = False
endsWithDelimiter PDFArray{}                      = True
endsWithDelimiter PDFDictionary{}                 = True
endsWithDelimiter PDFIndirectObject{}             = True
endsWithDelimiter PDFIndirectObjectWithStream{}   = True
endsWithDelimiter PDFIndirectObjectWithGraphics{} = True
endsWithDelimiter PDFObjectStream{}               = True
endsWithDelimiter PDFBool{}                       = False
endsWithDelimiter PDFNull                         = False
endsWithDelimiter PDFXRef{}                       = False
endsWithDelimiter PDFTrailer{}                    = True
endsWithDelimiter PDFStartXRef{}                  = True

{- |
Indicates whether the `PDFObject` starts with a delimiter when converted to a
`ByteString`.
-}
startsWithDelimiter :: PDFObject -> Bool
startsWithDelimiter PDFComment{}                    = True
startsWithDelimiter PDFVersion{}                    = True
startsWithDelimiter PDFEndOfFile                    = True
startsWithDelimiter PDFNumber{}                     = False
startsWithDelimiter PDFKeyword{}                    = False
startsWithDelimiter PDFName{}                       = True
startsWithDelimiter PDFString{}                     = True
startsWithDelimiter PDFHexString{}                  = True
startsWithDelimiter PDFReference{}                  = False
startsWithDelimiter PDFArray{}                      = True
startsWithDelimiter PDFDictionary{}                 = True
startsWithDelimiter PDFIndirectObject{}             = False
startsWithDelimiter PDFIndirectObjectWithStream{}   = False
startsWithDelimiter PDFIndirectObjectWithGraphics{} = False
startsWithDelimiter PDFObjectStream{}               = False
startsWithDelimiter PDFBool{}                       = False
startsWithDelimiter PDFNull                         = False
startsWithDelimiter PDFXRef{}                       = False
startsWithDelimiter PDFTrailer{}                    = False
startsWithDelimiter PDFStartXRef{}                  = False

{- |
Tells if a space must be inserted between 2 `PDFObject` when converted to
`ByteString`.
-}
spaceIfNeeded :: PDFObject -> PDFObject -> BS.ByteString
spaceIfNeeded object1 object2 | endsWithDelimiter object1   = ""
                              | startsWithDelimiter object2 = ""
                              | otherwise                   = " "

{- |
Converts a `PDFObject` to a `ByteString` ready to be inserted in an output
PDF file.
-}
fromPDFObject :: PDFObject -> BS.ByteString
fromPDFObject (PDFComment comment)     = BS.concat ["%", comment, "\n"]
fromPDFObject (PDFVersion version)     = BS.concat ["%PDF-", version, "\n"]
fromPDFObject PDFEndOfFile             = "%%EOF\n"
fromPDFObject (PDFNumber    number   ) = fromNumber number
fromPDFObject (PDFKeyword   keyword  ) = keyword
fromPDFObject (PDFName      name     ) = fromName name
fromPDFObject (PDFString    bytes    ) = fromString bytes
fromPDFObject (PDFHexString hexstring) = fromHexString hexstring
fromPDFObject (PDFReference number revision) =
  BS.concat [fromInt number, " ", fromInt revision, " R"]
fromPDFObject (PDFArray      objects   ) = fromArray objects
fromPDFObject (PDFDictionary dictionary) = fromDictionary dictionary
fromPDFObject (PDFIndirectObject number revision object) =
  fromIndirectObject number revision object
fromPDFObject (PDFIndirectObjectWithStream number revision dict stream) =
  fromIndirectObjectWithStream number revision dict stream
fromPDFObject (PDFIndirectObjectWithGraphics number revision dict gfx) =
  fromIndirectObjectWithGraphics number revision dict gfx
fromPDFObject (PDFObjectStream number revision dict stream) =
  fromIndirectObjectWithStream number revision dict stream
fromPDFObject (PDFBool True ) = "true"
fromPDFObject (PDFBool False) = "false"
fromPDFObject PDFNull         = "null"
fromPDFObject (PDFXRef xrss)  = fromXRef xrss
fromPDFObject (PDFTrailer (PDFDictionary dictionary)) =
  BS.concat ["trailer\n", fromDictionary dictionary, "\n"]
fromPDFObject (PDFTrailer _) = error "a trailer can only contain a dictionary"
fromPDFObject (PDFStartXRef offset) =
  BS.concat ["startxref\n", fromInt offset, "\n"]

{- |
Returns a `Text` string describing a PDFObject.
-}
objectInfo :: PDFObject -> TL.Text
objectInfo (PDFComment comment) = format ("{- " % utf8 % " -}") comment
objectInfo (PDFVersion version) = format ("VERSION=" % utf8) version
objectInfo PDFEndOfFile         = "END-OF-FILE"
objectInfo (PDFNumber number) =
  format ("[number:" % utf8 % "]") (fromNumber number)
objectInfo (PDFKeyword keyword) = format ("[keyword:" % utf8 % "]") keyword
objectInfo (PDFName    name   ) = format ("[name:" % utf8 % "]") name
objectInfo (PDFString  bytes  ) = format ("[string:" % utf8 % "]") bytes
objectInfo (PDFHexString hexstring) =
  format ("[hexstring: " % utf8 % "]") hexstring
objectInfo (PDFReference number revision) =
  format ("[ref:" % int % "," % int % "]") number revision
objectInfo (PDFArray objects) =
  format ("[array:count=" % int % "]") (length objects)
objectInfo (PDFDictionary dictionary) = case objectType dictionary of
  Just value -> format ("[dictionary:type=" % utf8 % ";count=" % int % "]")
                       value
                       (Map.size dictionary)
  Nothing -> format ("[dictionary:count=" % int % "]") (Map.size dictionary)
objectInfo (PDFIndirectObject number revision object) = format
  ("object(" % int % "," % int % ")=" % text)
  number
  revision
  (objectInfo object)
objectInfo (PDFIndirectObjectWithStream number revision dict stream) = format
  ("object(" % int % "," % int % ")=" % text % "+[stream:length=" % int % "]")
  number
  revision
  (objectInfo . PDFDictionary $ dict)
  (BS.length stream)
objectInfo (PDFIndirectObjectWithGraphics number revision dict _) = format
  ("graphics(" % int % "," % int % ")=" % text % "]")
  number
  revision
  (objectInfo . PDFDictionary $ dict)
objectInfo (PDFObjectStream number revision object stream) = format
  ( "objectstream("
  % int
  % ","
  % int
  % ")="
  % text
  % "+[stream:length="
  % int
  % "]"
  )
  number
  revision
  (objectInfo . PDFDictionary $ object)
  (BS.length stream)
objectInfo (PDFBool True ) = "true"
objectInfo (PDFBool False) = "false"
objectInfo PDFNull         = "null"
objectInfo xref@(PDFXRef _) =
  format ("[xref:count=" % int % "]") (xrefCount xref)
objectInfo (PDFTrailer dictionary@(PDFDictionary _)) =
  format ("trailer(" % text % ")") (objectInfo dictionary)
objectInfo (PDFTrailer _) = "<trailer without dictionary>"
objectInfo (PDFStartXRef offset) =
  format ("[startxref:offset=" % int % "]") offset

{- |
Takes a `List` of `PDFObject`, converts them to the `ByteString` representation
and inserts spaces between them if necessary.
-}
separateObjects :: [PDFObject] -> BS.ByteString
separateObjects []                           = ""
separateObjects [object1                   ] = fromPDFObject object1
separateObjects (object1 : object2 : others) = BS.concat
  [ fromPDFObject object1
  , spaceIfNeeded object1 object2
  , separateObjects (object2 : others)
  ]

fromXRefEntry :: XRefEntry -> BS.ByteString
fromXRefEntry xre = encodeUtf8 . TL.toStrict $ format
  (left 10 '0' % " " % left 5 '0' % " " % utf8 % "\r\n")
  (xreOffset xre)
  (xreGeneration xre)
  (if xreState xre == InUseEntry then "n" else "f")

fromXRefSubsection :: XRefSubsection -> BS.ByteString
fromXRefSubsection xrss = BS.concat
  [encodeUtf8 subsectionInfo, BS.concat (fromXRefEntry <$> xrssEntries xrss)]
 where
  subsectionInfo :: TS.Text
  subsectionInfo = TL.toStrict
    $ format ("" % int % " " % int % "\n") (xrssStart xrss) (xrssCount xrss)

fromXRef :: [XRefSubsection] -> BS.ByteString
fromXRef xrss = BS.concat ["xref\n", BS.concat (fmap fromXRefSubsection xrss)]

fromArray :: Array PDFObject -> BS.ByteString
fromArray items = BS.concat ["[", separateObjects (toList items), "]"]

fromDictionary :: Dictionary PDFObject -> BS.ByteString
fromDictionary keyValues = BS.concat
  ["<<", separateObjects (splitCouple (Map.toList keyValues)), ">>"]
 where
  splitCouple [] = []
  splitCouple ((key, value) : remains) =
    PDFName key : value : splitCouple remains

fromIndirectObject :: Int -> Int -> PDFObject -> BS.ByteString
fromIndirectObject number revision object = BS.concat
  [ BSU.fromString (show number)
  , " "
  , BSU.fromString (show revision)
  , " obj"
  , spaceIfNeeded (PDFKeyword "obj") object
  , fromPDFObject object
  , spaceIfNeeded object (PDFKeyword "endobj")
  , "endobj\n"
  ]

fromIndirectObjectWithStream
  :: Int -> Int -> Dictionary PDFObject -> BS.ByteString -> BS.ByteString
fromIndirectObjectWithStream number revision dict stream = BS.concat
  [ BSU.fromString (show number)
  , " "
  , BSU.fromString (show revision)
  , " obj"
  , spaceIfNeeded (PDFKeyword "obj") mkEmptyPDFDictionary
  , fromDictionary dict
  , spaceIfNeeded mkEmptyPDFDictionary (PDFKeyword "stream")
  , "stream\n"
  , stream
  , "\nendstream"
  , " endobj\n"
  ]

fromIndirectObjectWithGraphics
  :: Int -> Int -> Dictionary PDFObject -> Array GFXObject -> BS.ByteString
fromIndirectObjectWithGraphics number revision dict gfx = BS.concat
  [ BSU.fromString (show number)
  , " "
  , BSU.fromString (show revision)
  , " obj"
  , spaceIfNeeded (PDFKeyword "obj") mkEmptyPDFDictionary
  , fromDictionary dict
  , spaceIfNeeded mkEmptyPDFDictionary (PDFKeyword "stream")
  , "stream\n"
  , separateGfx gfx
  , "\nendstream"
  , " endobj\n"
  ]

{- |
Update the stream embedded in a `PDFObject`.

It also updates the Length entry in the associated dictionary to reflect the
change.
-}
updateStream :: PDFObject -> BS.ByteString -> PDFObject
updateStream object newStream = case object of
  (PDFIndirectObjectWithStream number revision dict _) ->
    PDFIndirectObjectWithStream number revision (newDict dict) newStream
  (PDFObjectStream number revision dict _) ->
    PDFObjectStream number revision (newDict dict) newStream
  _anyOtherObject -> object
 where
  newLength :: PDFObject
  newLength = PDFNumber . fromIntegral . BS.length $ newStream

  newDict :: Dictionary PDFObject -> Dictionary PDFObject
  newDict = Map.adjust (const newLength) "Length"

{- |
Returns the count of cross-references in a `PDFXRef` object.

If the object is not a `PDFXRef`, it returns 0.
-}
xrefCount :: PDFObject -> Int
xrefCount (PDFXRef subsections) = foldl' (+) 0 $ xrssCount <$> subsections
xrefCount _                     = 0

{- |
Given a `Dictionary`, generate a human `ByteString` describing the object based
on the Type and Subtype keys.

If the dictionary contains does not contain a Type key, it returns `Nothing`.
-}
objectType :: Dictionary PDFObject -> Maybe BS.ByteString
objectType dictionary =
  case (Map.lookup "Type" dictionary, Map.lookup "Type" dictionary) of
    (Just (PDFName typeValue), Just (PDFName subtypeValue)) ->
      Just $ BS.concat [typeValue, "/", subtypeValue]
    (Just (PDFName typeValue), _noSubtype) -> Just typeValue
    _noType -> Nothing

{- |
Determine if a key is in a dictionary from a `PDFObject`.

If the `PDFObject` has no dictionary, it returns `False`.
-}
hasKey
  :: BS.ByteString -- ^ The key to search for
  -> PDFObject -- ^ The `PDFObject` to search in
  -> Bool
hasKey key (PDFDictionary dict                        ) = dictHasKey key dict
hasKey key (PDFIndirectObjectWithStream _ _ dict _    ) = dictHasKey key dict
hasKey key (PDFObjectStream             _ _ dict _    ) = dictHasKey key dict
hasKey key (PDFIndirectObject _ _ (PDFDictionary dict)) = dictHasKey key dict
hasKey key (PDFTrailer (PDFDictionary dict)           ) = dictHasKey key dict
hasKey _   _anyOtherObject                              = False

{- |
Determine if a `PDFObject` has a dictionary.
-}
hasDictionary :: PDFObject -> Bool
hasDictionary (PDFIndirectObject _ _ (PDFDictionary _)) = True
hasDictionary PDFIndirectObjectWithStream{}             = True
hasDictionary PDFObjectStream{}                         = True
hasDictionary PDFDictionary{}                           = True
hasDictionary (PDFTrailer (PDFDictionary _))            = True
hasDictionary _anyOtherObject                           = False

{- |
Determine if a `PDFObject` has a stream.
-}
hasStream :: PDFObject -> Bool
hasStream PDFIndirectObjectWithStream{} = True
hasStream PDFObjectStream{}             = True
hasStream _anyOtherObject               = False
