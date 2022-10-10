{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE InstanceSigs #-}

{-|
This module defines what is a GFX object and functions in relation with the
PDF specification.
-}
module Pdf.Graphics.Object
  ( -- * GFX object
    GFXObject
    ( GFXComment
    , GFXNumber
    , GFXKeyword
    , GFXName
    , GFXString
    , GFXHexString
    , GFXReference
    , GFXArray
    , GFXDictionary
    , GFXBool
    , GFXNull
    )
  , Dictionary
  , mkDictionary
  , mkEmptyDictionary
  , mkEmptyGFXArray
  , mkEmptyGFXDictionary
  , mkGFXArray
  , mkGFXDictionary

    -- * Conversion
  , fromGFXObject

    -- * GFX characters
  , isDelimiter
  , isPlusMinus
  , isWhiteSpace
  , isKeywordCharacter
  , isOctal
  , isStringEscapeSequence
  , isStringRegularChar
  , isNameRegularChar
  , spaceIfNeeded
  ) where

import qualified Data.ByteString               as BS
import qualified Data.Text.Lazy                as TL
import qualified Data.Sequence                 as SQ
import           Formatting                     ( format
                                                , (%)
                                                , int
                                                , text
                                                )
import           Formatting.ByteStringFormatter ( utf8 )
import qualified Data.Map.Strict               as Map
import           Data.Ix                        ( inRange )
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

{-|
Test if a byte is a GFX delimiter.

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
Test if a byte is a valid escaped character in a GFX string.

The following characters are considered valid: n, r, t, b, f, (, ) and \\.
-}
isStringEscapeSequence :: Word8 -> Bool
isStringEscapeSequence = isJust . flip BS.elemIndex "nrtbf()\\"

{-|
Test if a byte is a valid regular character in a GFX string.

Any character that is not (, ) or \\ is a valid regular character.
-}
isStringRegularChar :: Word8 -> Bool
isStringRegularChar = isNothing . flip BS.elemIndex "()\\"

{-|
Test if a byte is a valid character for a GFX name.

A GFX name may not contain an `asciiNUL` character, white space or delimiter.

The `asciiNUMBERSIGN` is reserved for escaping.
-}
isNameRegularChar :: Word8 -> Bool
isNameRegularChar byte | byte == asciiNUMBERSIGN = False
                       | byte == asciiNUL        = False
                       | isWhiteSpace byte       = False
                       | isDelimiter byte        = False
                       | otherwise               = True

{- |
A `Dictionary` is a handy type.

It is a `Map` of `GFXObject` indexed by `ByteString`.

The keys are the `ByteString` contained in `GFXName`.
-}
type Dictionary = Map.Map BS.ByteString GFXObject

-- | Returns an empty `Dictionary`
mkEmptyDictionary :: Dictionary
mkEmptyDictionary = Map.empty

mkDictionary :: [(BS.ByteString, GFXObject)] -> Dictionary
mkDictionary = Map.fromList

data GFXLineCap = GFXButtCap | GFXRoundCap | GFXProjectingSquareCap deriving stock (Eq, Show, Enum)
data GFXLineJoin = GFXMiterJoin | GFXRoundJoin | GFXBevelJoin deriving stock (Eq, Show, Enum)

data GFXTextRenderMode
  = GFXFillText
  | GFXStrokeText
  | GFXInvisibleText
  | GFXFillTextClipping
  | GFXStrokeTextClipping
  | GFXFillStrokeTextClipping
  | GFXTextClipping
  deriving stock (Eq, Show, Enum)

data GFXColorSpace
  = GFXDeviceGray
  | GFXDeviceRGB
  | GFXDeviceCMYK
  | GFXPattern
  | GFXLab
  | GFXICCBased
  | GFXIndexed
  | GFXDeviceN
  deriving stock (Eq, Show)

data GSOperator 
  = -- | Save the current graphics state on the graphics state stack (q)
    GSSaveGS
    -- | Restore the graphics state (Q)
  | GSRestoreGS
    -- | Modify the current transformation matrix (cm)
  | GSSetCTM
    -- | Set the line width in the graphics state (w)
  | GSSetLineWidth
    -- | Set the line cap style in the graphics state (J)
  | GSSetLineCap
    -- | Set the line join style in the graphics state (j)
  | GSSetLineJoin
    -- | Set the miter limit in the graphics state (M)
  | GSSetMiterLimit
    -- | Set the line dash pattern in the graphics state (d)
  | GSSetLineDashPattern
    -- | Set the colour rendering intent in the graphics state (ri)
  | GSSetColourRenderingIntent
    -- | Set the flatness tolerance in the graphics state (i)
  | GSSetFlatnessTolerance
    -- | Set the specified parameters in the graphics state (gs)
  | GSSetParameters
    -- | Begin a new subpath by moving the current point to coordinates (m)
  | GSMoveTo
    -- | Append a straight line segment from the current point to the point (l)
  | GSLineTo
    -- | Append a cubic Bézier curve to the current path (c)
  | GSCubicBezierCurve
  | GSCubicBezierCurve1To
  | GSCubicBezierCurve2To
  | GSCloseSubpath
  | GSRectangle
  | GSStrokePath
  | GSCloseStrokePath
  | GSFillPathNZWR
  | GSFilePathEOR
  | GSFillStrokePathNZWR
  | GSFillStrokePathEOR
  | GSCloseFillStrokeNZWR
  | GSCloseFillStrokeEOR
  | GSEndPath
  | GSBeginText
  | GSEndText
  | GSMoveToNextLine
  | GSMoveToNextLineLP
  | GSSetTextMatrix
  | GSNextLine
  | GSShowText
  | GSNLShowText
  | GSNLShowTextWithSpacing
  | GSShowManyText
  | GSSetCharacterSpacing
  | GSSetWordSpacing
  | GSSetHorizontalScaling
  | GSSetTextLeading
  | GSSetTextFont
  | GSSetTextRenderingMode
  | GSSetTextRise
  | GSSetGlyphWidth
  | GSSetBoundingBoxGlyph
  | GSSetStrokeColorspace
  | GSSetNonStrokeColorspace
  | GSSetStrokeColor
  | GSSetStrokeColorN
  | GSSetNonStrokeColor
  | GSSetNonStrockeColorN
  | GSSetStrokeGrayColorspace
  | GSSetNonStrokeGrayColorspace
  | GSSetStrokeRGBColorspace
  | GSSetNonStrokeRGBColorspace
  | GSSetStrokeCMYKColorspace
  | GSSetNonStrokeCMYKColorspace
  deriving stock (Eq, Show)

{-|
A GFX is a collection of objects, here named GFX objects.

Values contained are decoded, meaning they no longer contain escape sequences.
-}
data GFXObject
  = -- | A comment (without the starting %)
    GFXComment BS.ByteString
  | -- | A number (always stored as a double)
    GFXNumber Double
  | -- | A keyword
    GFXKeyword BS.ByteString
  | -- | A name (starting with /)
    GFXName BS.ByteString
  | -- | A string (unescaped and without parenthesis)
    GFXString BS.ByteString
  | -- | An hexadeicmal string (without less-than/greater-than signs)
    GFXHexString BS.ByteString
  | -- | A reference, number and generation (two integers followed by an `R`)
    GFXReference Int Int
  | -- | An array containing a list of objects
    GFXArray (SQ.Seq GFXObject)
  | -- | A dictionary containing key-value pairs
    GFXDictionary Dictionary
  | -- | A boolean (true or false)
    GFXBool Bool
  | -- | A null value
    GFXNull
  | -- | An operator
    GFXOperator GSOperator
  deriving stock (Eq, Show)

{- |
Create an empty `GFXDictionary`.
-}
mkEmptyGFXDictionary :: GFXObject
mkEmptyGFXDictionary = GFXDictionary Map.empty

{- |
Create an empty `GFXArray`.
-}
mkEmptyGFXArray :: GFXObject
mkEmptyGFXArray = GFXArray SQ.empty

{- |
Create a `GFXDictionary` from a list of couples (key, value).
-}
mkGFXDictionary :: [(BS.ByteString, GFXObject)] -> GFXObject
mkGFXDictionary = GFXDictionary . Map.fromList

{- |
Create a `GFXArray` from a list of `GFXObject`.
-}
mkGFXArray :: [GFXObject] -> GFXObject
mkGFXArray = GFXArray . SQ.fromList

instance Eq GFXObject where
  (==) :: GFXObject -> GFXObject -> Bool
  (GFXComment x)       == (GFXComment y)       = x == y
  (GFXVersion _)       == (GFXVersion _)       = True
  GFXEndOfFile         == GFXEndOfFile         = True
  (GFXNumber    x    ) == (GFXNumber    y    ) = x == y
  (GFXKeyword   x    ) == (GFXKeyword   y    ) = x == y
  (GFXName      x    ) == (GFXName      y    ) = x == y
  (GFXString    x    ) == (GFXString    y    ) = x == y
  (GFXHexString x    ) == (GFXHexString y    ) = x == y
  (GFXReference xn xr) == (GFXReference yn yr) = xn == yn && xr == yr
  (GFXArray      x   ) == (GFXArray      y   ) = x == y
  (GFXDictionary x   ) == (GFXDictionary y   ) = x == y
  (GFXIndirectObject xn xr _) == (GFXIndirectObject yn yr _) =
    xn == yn && xr == yr
  (GFXIndirectObjectWithStream xn xr _ _) == (GFXIndirectObjectWithStream yn yr _ _)
    = xn == yn && xr == yr
  (GFXObjectStream xn xr _ _) == (GFXObjectStream yn yr _ _) =
    xn == yn && xr == yr
  (GFXBool x)      == (GFXBool y)      = x == y
  GFXNull          == GFXNull          = True
  (GFXXRef      x) == (GFXXRef      y) = x == y
  (GFXTrailer   x) == (GFXTrailer   y) = x == y
  (GFXStartXRef x) == (GFXStartXRef y) = x == y
  _anyObjectA      == _anyObjectB      = False

objectRank :: GFXObject -> Int
objectRank (GFXVersion _)                = 0
objectRank (GFXComment _)                = 1
objectRank GFXNull                       = 2
objectRank (GFXBool      _  )            = 3
objectRank (GFXNumber    _  )            = 4
objectRank (GFXKeyword   _  )            = 5
objectRank (GFXName      _  )            = 6
objectRank (GFXString    _  )            = 7
objectRank (GFXHexString _  )            = 8
objectRank (GFXReference _ _)            = 9
objectRank (GFXArray      _ )            = 10
objectRank (GFXDictionary _ )            = 11
objectRank GFXIndirectObject{}           = 12
objectRank GFXIndirectObjectWithStream{} = 13
objectRank GFXObjectStream{}             = 14
objectRank (GFXXRef      _)              = 15
objectRank (GFXTrailer   _)              = 16
objectRank (GFXStartXRef _)              = 17
objectRank GFXEndOfFile                  = 18

instance Ord GFXObject where
  compare :: GFXObject -> GFXObject -> Ordering
  compare (GFXComment x)   (GFXComment y)   = compare x y
  compare (GFXVersion _)   (GFXVersion _)   = EQ
  compare GFXEndOfFile     GFXEndOfFile     = EQ
  compare (GFXNumber    x) (GFXNumber    y) = compare x y
  compare (GFXKeyword   x) (GFXKeyword   y) = compare x y
  compare (GFXName      x) (GFXName      y) = compare x y
  compare (GFXString    x) (GFXString    y) = compare x y
  compare (GFXHexString x) (GFXHexString y) = compare x y
  compare (GFXReference xn xr) (GFXReference yn yr) =
    compare xn yn <> compare xr yr
  compare (GFXArray      x) (GFXArray      y) = compare x y
  compare (GFXDictionary x) (GFXDictionary y) = compare x y
  compare (GFXIndirectObject xn xr _) (GFXIndirectObject yn yr _) =
    compare xn yn <> compare xr yr
  compare (GFXIndirectObjectWithStream xn xr _ _) (GFXIndirectObjectWithStream yn yr _ _)
    = compare xn yn <> compare xr yr
  compare (GFXObjectStream xn xr _ _) (GFXObjectStream yn yr _ _) =
    compare xn yn <> compare xr yr
  compare (GFXBool x)      (GFXBool y)      = compare x y
  compare GFXNull          GFXNull          = EQ
  compare (GFXXRef      x) (GFXXRef      y) = compare x y
  compare (GFXTrailer   x) (GFXTrailer   y) = compare x y
  compare (GFXStartXRef x) (GFXStartXRef y) = compare x y
  compare objectA objectB = compare (objectRank objectA) (objectRank objectB)

{- |
Indicates whether the `GFXObject` ends with a delimiter when converted to a
`ByteString`.
-}
endsWithDelimiter :: GFXObject -> Bool
endsWithDelimiter GFXComment{}                  = True
endsWithDelimiter GFXVersion{}                  = True
endsWithDelimiter GFXEndOfFile                  = True
endsWithDelimiter GFXNumber{}                   = False
endsWithDelimiter GFXKeyword{}                  = False
endsWithDelimiter GFXName{}                     = False
endsWithDelimiter GFXString{}                   = True
endsWithDelimiter GFXHexString{}                = True
endsWithDelimiter GFXReference{}                = False
endsWithDelimiter GFXArray{}                    = True
endsWithDelimiter GFXDictionary{}               = True
endsWithDelimiter GFXIndirectObject{}           = True
endsWithDelimiter GFXIndirectObjectWithStream{} = True
endsWithDelimiter GFXObjectStream{}             = True
endsWithDelimiter GFXBool{}                     = False
endsWithDelimiter GFXNull                       = False
endsWithDelimiter GFXXRef{}                     = False
endsWithDelimiter GFXTrailer{}                  = True
endsWithDelimiter GFXStartXRef{}                = True

{- |
Indicates whether the `GFXObject` starts with a delimiter when converted to a
`ByteString`.
-}
startsWithDelimiter :: GFXObject -> Bool
startsWithDelimiter GFXComment{}                  = True
startsWithDelimiter GFXVersion{}                  = True
startsWithDelimiter GFXEndOfFile                  = True
startsWithDelimiter GFXNumber{}                   = False
startsWithDelimiter GFXKeyword{}                  = False
startsWithDelimiter GFXName{}                     = True
startsWithDelimiter GFXString{}                   = True
startsWithDelimiter GFXHexString{}                = True
startsWithDelimiter GFXReference{}                = False
startsWithDelimiter GFXArray{}                    = True
startsWithDelimiter GFXDictionary{}               = True
startsWithDelimiter GFXIndirectObject{}           = False
startsWithDelimiter GFXIndirectObjectWithStream{} = False
startsWithDelimiter GFXObjectStream{}             = False
startsWithDelimiter GFXBool{}                     = False
startsWithDelimiter GFXNull                       = False
startsWithDelimiter GFXXRef{}                     = False
startsWithDelimiter GFXTrailer{}                  = False
startsWithDelimiter GFXStartXRef{}                = False

{- |
Tells if a space must be inserted between 2 `GFXObject` when converted to
`ByteString`.
-}
spaceIfNeeded :: GFXObject -> GFXObject -> BS.ByteString
spaceIfNeeded object1 object2 | endsWithDelimiter object1   = ""
                              | startsWithDelimiter object2 = ""
                              | otherwise                   = " "

{- |
Converts a `GFXObject` to a `ByteString` ready to be inserted in an output
GFX file.
-}
fromGFXObject :: GFXObject -> BS.ByteString
fromGFXObject (GFXComment comment)     = BS.concat ["%", comment, "\n"]
fromGFXObject (GFXVersion version)     = BS.concat ["%GFX-", version, "\n"]
fromGFXObject GFXEndOfFile             = "%%EOF\n"
fromGFXObject (GFXNumber    number   ) = fromNumber number
fromGFXObject (GFXKeyword   keyword  ) = keyword
fromGFXObject (GFXName      name     ) = fromName name
fromGFXObject (GFXString    bytes    ) = fromString bytes
fromGFXObject (GFXHexString hexstring) = fromHexString hexstring
fromGFXObject (GFXReference number revision) =
  BS.concat [fromInt number, " ", fromInt revision, " R"]
fromGFXObject (GFXArray      objects   ) = fromArray objects
fromGFXObject (GFXDictionary dictionary) = fromDictionary dictionary
fromGFXObject (GFXIndirectObject number revision object) =
  fromIndirectObject number revision object
fromGFXObject (GFXIndirectObjectWithStream number revision dict stream) =
  fromIndirectObjectWithStream number revision dict stream
fromGFXObject (GFXObjectStream number revision dict stream) =
  fromIndirectObjectWithStream number revision dict stream
fromGFXObject (GFXBool True ) = "true"
fromGFXObject (GFXBool False) = "false"
fromGFXObject GFXNull         = "null"
fromGFXObject (GFXXRef xrss)  = fromXRef xrss
fromGFXObject (GFXTrailer (GFXDictionary dictionary)) =
  BS.concat ["trailer\n", fromDictionary dictionary, "\n"]
fromGFXObject (GFXTrailer _) = error "a trailer can only contain a dictionary"
fromGFXObject (GFXStartXRef offset) =
  BS.concat ["startxref\n", fromInt offset, "\n"]

{- |
Returns a `Text` string describing a GFXObject.
-}
objectInfo :: GFXObject -> TL.Text
objectInfo (GFXComment comment) = format ("{- " % utf8 % " -}") comment
objectInfo (GFXVersion version) = format ("VERSION=" % utf8) version
objectInfo GFXEndOfFile         = "END-OF-FILE"
objectInfo (GFXNumber number) =
  format ("[number:" % utf8 % "]") (fromNumber number)
objectInfo (GFXKeyword keyword) = format ("[keyword:" % utf8 % "]") keyword
objectInfo (GFXName    name   ) = format ("[name:" % utf8 % "]") name
objectInfo (GFXString  bytes  ) = format ("[string:" % utf8 % "]") bytes
objectInfo (GFXHexString hexstring) =
  format ("[hexstring: " % utf8 % "]") hexstring
objectInfo (GFXReference number revision) =
  format ("[ref:" % int % "," % int % "]") number revision
objectInfo (GFXArray objects) =
  format ("[array:count=" % int % "]") (length objects)
objectInfo (GFXDictionary dictionary) = case objectType dictionary of
  Just value -> format ("[dictionary:type=" % utf8 % ";count=" % int % "]")
                       value
                       (Map.size dictionary)
  Nothing -> format ("[dictionary:count=" % int % "]") (Map.size dictionary)
objectInfo (GFXIndirectObject number revision object) = format
  ("object(" % int % "," % int % ")=" % text)
  number
  revision
  (objectInfo object)
objectInfo (GFXIndirectObjectWithStream number revision dict stream) = format
  ("object(" % int % "," % int % ")=" % text % "+[stream:length=" % int % "]")
  number
  revision
  (objectInfo . GFXDictionary $ dict)
  (BS.length stream)
objectInfo (GFXObjectStream number revision object stream) = format
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
  (objectInfo . GFXDictionary $ object)
  (BS.length stream)
objectInfo (GFXBool True ) = "true"
objectInfo (GFXBool False) = "false"
objectInfo GFXNull         = "null"
objectInfo xref@(GFXXRef _) =
  format ("[xref:count=" % int % "]") (xrefCount xref)
objectInfo (GFXTrailer dictionary@(GFXDictionary _)) =
  format ("trailer(" % text % ")") (objectInfo dictionary)
objectInfo (GFXTrailer _) = "<trailer without dictionary>"
objectInfo (GFXStartXRef offset) =
  format ("[startxref:offset=" % int % "]") offset

{- |
Takes a `List` of `GFXObject`, converts them to the `ByteString` representation
and inserts spaces between them if necessary.
-}
separateObjects :: [GFXObject] -> BS.ByteString
separateObjects []                           = ""
separateObjects [object1                   ] = fromGFXObject object1
separateObjects (object1 : object2 : others) = BS.concat
  [ fromGFXObject object1
  , spaceIfNeeded object1 object2
  , separateObjects (object2 : others)
  ]

fromArray :: SQ.Seq GFXObject -> BS.ByteString
fromArray items = BS.concat ["[", separateObjects (toList items), "]"]

fromDictionary :: Dictionary -> BS.ByteString
fromDictionary keyValues = BS.concat
  ["<<", separateObjects (splitCouple (Map.toList keyValues)), ">>"]
 where
  splitCouple [] = []
  splitCouple ((key, value) : remains) =
    GFXName key : value : splitCouple remains
