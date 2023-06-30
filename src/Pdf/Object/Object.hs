{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StrictData #-}
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
    , PDFBool
    , PDFNull
    , PDFXRef
    , PDFTrailer
    , PDFStartXRef
    )
  , Dictionary
  , fromPDFObject
  , objectInfo
  , getValue

    -- * PDF indirect object
  , updateStream

    -- * PDF characters
  , isDelimiter
  , isPlusMinus
  , isWhiteSpace
  , isSpace
  , isKeywordCharacter
  , isOctal
  , isStringEscapeSequence
  , isStringRegularChar
  , isNameRegularChar
  , spaceIfNeeded
  ,

    -- * XRef
    xrefCount
  , inUseEntry
  , freeEntry
  , XRefState(InUseEntry, FreeEntry)
  , XRefEntry(XRefEntry, xreOffset, xreGeneration, xreState)
  , XRefSubsection(XRefSubsection, xrssStart, xrssCount, xrssEntries)
  ) where

import qualified Data.ByteString               as BS
import qualified Data.ByteString.UTF8          as BSU
import qualified Data.Text.Lazy                as TL
import           Formatting                     ( format
                                                , (%)
                                                , int
                                                , text
                                                )
import           Formatting.ByteStringFormatter ( utf8 )
import qualified Data.HashMap.Strict           as HM
import           Data.Ix                        ( inRange )
import           Data.List                      ( foldl' )
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
Test if a byte is a space.

The following characters are considered white spaces:

- `asciiNUL`
- `asciiTAB`
- `asciiSPACE`
-}
isSpace :: Word8 -> Bool
isSpace = isJust . flip BS.elemIndex "\0\t "

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

data XRefStmFieldType =
    XRSFFreeEntry
  | XRSFUncompressedObject
  | XRSFCompressedObject
  deriving stock (Eq, Show)

type Dictionary = HM.HashMap BS.ByteString PDFObject

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
    PDFArray [PDFObject]
  | -- | A dictionary containing key-value pairs
    PDFDictionary Dictionary
  | -- | An indirect object, object number, generation, object itself and stream
    PDFIndirectObject Int Int PDFObject (Maybe BS.ByteString)
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
  deriving stock (Eq, Show)

endsWithDelimiter :: PDFObject -> Bool
endsWithDelimiter PDFComment{}        = True
endsWithDelimiter PDFVersion{}        = True
endsWithDelimiter PDFEndOfFile        = True
endsWithDelimiter PDFNumber{}         = False
endsWithDelimiter PDFKeyword{}        = False
endsWithDelimiter PDFName{}           = False
endsWithDelimiter PDFString{}         = True
endsWithDelimiter PDFHexString{}      = True
endsWithDelimiter PDFReference{}      = False
endsWithDelimiter PDFArray{}          = True
endsWithDelimiter PDFDictionary{}     = True
endsWithDelimiter PDFIndirectObject{} = True
endsWithDelimiter PDFBool{}           = False
endsWithDelimiter PDFNull             = False
endsWithDelimiter PDFXRef{}           = False
endsWithDelimiter PDFTrailer{}        = True
endsWithDelimiter PDFStartXRef{}      = True

startsWithDelimiter :: PDFObject -> Bool
startsWithDelimiter PDFComment{}        = True
startsWithDelimiter PDFVersion{}        = True
startsWithDelimiter PDFEndOfFile        = True
startsWithDelimiter PDFNumber{}         = False
startsWithDelimiter PDFKeyword{}        = False
startsWithDelimiter PDFName{}           = True
startsWithDelimiter PDFString{}         = True
startsWithDelimiter PDFHexString{}      = True
startsWithDelimiter PDFReference{}      = False
startsWithDelimiter PDFArray{}          = True
startsWithDelimiter PDFDictionary{}     = True
startsWithDelimiter PDFIndirectObject{} = False
startsWithDelimiter PDFBool{}           = False
startsWithDelimiter PDFNull             = False
startsWithDelimiter PDFXRef{}           = False
startsWithDelimiter PDFTrailer{}        = False
startsWithDelimiter PDFStartXRef{}      = False

spaceIfNeeded :: PDFObject -> PDFObject -> BS.ByteString
spaceIfNeeded object1 object2 | endsWithDelimiter object1   = ""
                              | startsWithDelimiter object2 = ""
                              | otherwise                   = " "

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
fromPDFObject (PDFIndirectObject number revision object stream) =
  fromIndirectObject number revision object stream
fromPDFObject (PDFBool True ) = "true"
fromPDFObject (PDFBool False) = "false"
fromPDFObject PDFNull         = "null"
fromPDFObject (PDFXRef subsections) =
  BS.concat ("xref\n" : (fromXRefSubsection <$> subsections))
fromPDFObject (PDFTrailer (PDFDictionary dictionary)) =
  BS.concat ["trailer\n", fromDictionary dictionary, "\n"]
fromPDFObject (PDFTrailer _) = error "a trailer can only contain a dictionary"
fromPDFObject (PDFStartXRef offset) =
  BS.concat ["startxref\n", fromInt offset, "\n"]

pad10 :: Int -> BS.ByteString
pad10 value = BS.drop (BS.length base) (BS.concat [pads, base])
 where
  base = fromInt value
  pads = "0000000000" :: BS.ByteString

pad5 :: Int -> BS.ByteString
pad5 value = BS.drop (BS.length base) (BS.concat [pads, base])
 where
  base = fromInt value
  pads = "00000" :: BS.ByteString

fromXRefEntry :: XRefEntry -> BS.ByteString
fromXRefEntry (XRefEntry offset generation InUseEntry) =
  BS.concat [pad10 offset, " ", pad5 generation, " n \n"]
fromXRefEntry (XRefEntry offset _ FreeEntry) =
  BS.concat [pad10 offset, " 65535 f \n"]

fromXRefSubsection :: XRefSubsection -> BS.ByteString
fromXRefSubsection (XRefSubsection start count entries) = BS.concat
  (fromInt start: " " : fromInt count : "\n" : (fromXRefEntry <$> entries))

{- | Returns a `Text` string describing a PDFObject.
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
                       (HM.size dictionary)
  Nothing -> format ("[dictionary:count=" % int % "]") (HM.size dictionary)
objectInfo (PDFIndirectObject number revision object Nothing) = format
  ("object(" % int % "," % int % ")=" % text)
  number
  revision
  (objectInfo object)
objectInfo (PDFIndirectObject number revision object (Just stream)) = format
  ("object(" % int % "," % int % ")=" % text % "+[stream:length=" % int % "]")
  number
  revision
  (objectInfo object)
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

separateObjects :: [PDFObject] -> BS.ByteString
separateObjects []                           = ""
separateObjects [object1                   ] = fromPDFObject object1
separateObjects (object1 : object2 : others) = BS.concat
  [ fromPDFObject object1
  , spaceIfNeeded object1 object2
  , separateObjects (object2 : others)
  ]

fromArray :: [PDFObject] -> BS.ByteString
fromArray items = BS.concat ["[", separateObjects items, "]"]

fromDictionary :: HM.HashMap BS.ByteString PDFObject -> BS.ByteString
fromDictionary keyValues = BS.concat
  ["<<", separateObjects (splitCouple (HM.toList keyValues)), ">>"]
 where
  splitCouple [] = []
  splitCouple ((key, value) : remains) =
    PDFName key : value : splitCouple remains

fromIndirectObject
  :: Int -> Int -> PDFObject -> Maybe BS.ByteString -> BS.ByteString
fromIndirectObject number revision object Nothing = BS.concat
  [ BSU.fromString (show number)
  , " "
  , BSU.fromString (show revision)
  , " obj"
  , spaceIfNeeded (PDFKeyword "obj") object
  , fromPDFObject object
  , spaceIfNeeded object (PDFKeyword "endobj")
  , "endobj\n"
  ]
fromIndirectObject number revision object (Just stream) = BS.concat
  [ BSU.fromString (show number)
  , " "
  , BSU.fromString (show revision)
  , " obj"
  , spaceIfNeeded (PDFKeyword "obj") object
  , fromPDFObject object
  , spaceIfNeeded object (PDFKeyword "stream")
  , "stream\n"
  , stream
  , "\nendstream"
  , " endobj\n"
  ]

updateStream :: PDFObject -> BS.ByteString -> PDFObject
updateStream (PDFIndirectObject number revision (PDFDictionary dictionary) _) stream
  = PDFIndirectObject number
                      revision
                      (PDFDictionary newDictionary)
                      (Just stream)
 where
  newLength :: PDFObject
  newLength = PDFNumber . fromIntegral . BS.length $ stream

  newDictionary :: HM.HashMap BS.ByteString PDFObject
  newDictionary = HM.adjust (const newLength) "Length" dictionary
updateStream anyOtherObject _ = anyOtherObject

xrefCount :: PDFObject -> Int
xrefCount (PDFXRef subsections) = foldl' (+) 0 $ xrssCount <$> subsections
xrefCount _                     = 0

objectType :: HM.HashMap BS.ByteString PDFObject -> Maybe BS.ByteString
objectType dictionary = case HM.lookup "Type" dictionary of
  Just (PDFName typeValue) -> case HM.lookup "Subtype" dictionary of
    Just (PDFName subtypeValue) ->
      Just $ BS.concat [typeValue, "/", subtypeValue]
    _anyOtherValue -> Just typeValue
  _anyOtherValue -> Nothing

-- | Get value in a dictionary from a `PDFObject`
getValue :: BS.ByteString -> PDFObject -> Maybe PDFObject
getValue name (PDFDictionary dictionary) = dictionary HM.!? name
getValue name (PDFIndirectObject _ _ (PDFDictionary dictionary) _) =
  dictionary HM.!? name
getValue name (PDFTrailer (PDFDictionary dictionary)) = dictionary HM.!? name
getValue _    _ = Nothing
