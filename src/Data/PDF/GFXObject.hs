{-|
This module defines what is a GFX object and functions in relation with the
PDF specification.
-}
module Data.PDF.GFXObject
  ( -- * GFX object
    GFXObject
    ( GFXComment
    , GFXNumber
    , GFXName
    , GFXString
    , GFXHexString
    , GFXReference
    , GFXArray
    , GFXDictionary
    , GFXBool
    , GFXNull
    , GFXOperator
    , GFXInlineImage
    )
  , mkEmptyGFXArray
  , mkEmptyGFXDictionary
  , mkGFXArray
  , mkGFXDictionary
  , objectInfo

    -- * Operators
  , GSOperator(..)

    -- * Conversion
  , fromGFXObject
  , toGSOperator
  , separateGfx
  , reducePrecision

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
  , isKeywordFirstCharacter
  ) where

import Data.Array (Array, mkArray, mkEmptyArray)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (toList)
import Data.Ix (inRange)
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, isNothing)
import Data.Sequence qualified as SQ
import Data.Text.Lazy qualified as TL
import Data.Word (Word8)

import Formatting (format, int, (%))
import Formatting.ByteStringFormatter (utf8)

import Util.Ascii
  ( asciiDIGITNINE
  , asciiDIGITSEVEN
  , asciiDIGITZERO
  , asciiLOWERA
  , asciiLOWERZ
  , asciiUPPERA
  , asciiUPPERZ
  , pattern AsciiAPOSTROPHE
  , pattern AsciiASTERISK
  , pattern AsciiCR
  , pattern AsciiFF
  , pattern AsciiHT
  , pattern AsciiLF
  , pattern AsciiNUL
  , pattern AsciiNUMBERSIGN
  , pattern AsciiQUOTATIONMARK
  , pattern AsciiSPACE
  )
import Util.Dictionary (Dictionary, mkDictionary, mkEmptyDictionary)
import Util.Name (fromName)
import Util.Number (fromInt, fromNumber, round')
import Util.String (fromHexString, fromString)

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
isWhiteSpace AsciiSPACE         = True
isWhiteSpace AsciiLF            = True
isWhiteSpace AsciiCR            = True
isWhiteSpace AsciiHT            = True
isWhiteSpace AsciiNUL           = True
isWhiteSpace AsciiFF            = True
isWhiteSpace _anyOtherCharacter = False

{-|
Test if a byte is a keyword character.

Keyword characters are either lowercase or uppercase alphabetical characters.
-}
isKeywordFirstCharacter :: Word8 -> Bool
isKeywordFirstCharacter AsciiASTERISK                                  = True
isKeywordFirstCharacter AsciiAPOSTROPHE                                = True
isKeywordFirstCharacter AsciiQUOTATIONMARK                             = True
isKeywordFirstCharacter byte | inRange (asciiLOWERA, asciiLOWERZ) byte = True
                             | inRange (asciiUPPERA, asciiUPPERZ) byte = True
                             | otherwise                               = False

isKeywordCharacter :: Word8 -> Bool
isKeywordCharacter AsciiASTERISK                                        = True
isKeywordCharacter AsciiAPOSTROPHE                                      = True
isKeywordCharacter AsciiQUOTATIONMARK                                   = True
isKeywordCharacter byte | inRange (asciiLOWERA, asciiLOWERZ) byte       = True
                        | inRange (asciiUPPERA, asciiUPPERZ) byte       = True
                        | inRange (asciiDIGITZERO, asciiDIGITNINE) byte = True
                        | otherwise                                     = False

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
isNameRegularChar AsciiNUMBERSIGN                = False
isNameRegularChar AsciiNUL                       = False
isNameRegularChar byte | isWhiteSpace byte       = False
                       | isDelimiter byte        = False
                       | otherwise               = True

-- | Line cap styles
type GFXLineCap :: Type
data GFXLineCap
  = -- | Butt cap (0)
    GFXButtCap
  | -- | Round cap (1)
    GFXRoundCap
    -- | Projecting square cap (2)
  | GFXProjectingSquareCap
  deriving stock (Eq, Show, Enum)

-- | Line join styles
type GFXLineJoin :: Type
data GFXLineJoin
  = -- | Mitter join (0)
    GFXMiterJoin
  | -- | Round join (1)
    GFXRoundJoin
    -- | Bevel join (2)
  | GFXBevelJoin
  deriving stock (Eq, Show, Enum)

-- | Text rendering mode
type GFXTextRenderMode :: Type
data GFXTextRenderMode
  = -- | Fill text (0)
    GFXFillText
  | -- | Stroke text (1)
    GFXStrokeText
  | -- | Fill then stroke text (2)
    GFXFillStrokeText
  | -- | Neither fill nor stroke text, invisible (3)
    GFXInvisibleText
  | -- | Fill text and add to path for clipping (4)
    GFXFillTextClipping
  | -- | Stroke text and add to path for clipping (5)
    GFXStrokeTextClipping
  | -- | Fill then stroke text and add path for clipping (6)
    GFXFillStrokeTextClipping
  | -- | Add text to path for clipping (7)
    GFXTextClipping
  deriving stock (Eq, Show, Enum)

-- | PDF colour spaces
type GFXColorSpace :: Type
data GFXColorSpace
  = -- | Gray colour space (/DeviceGray)
    GFXDeviceGray
  | -- | RGB colour space (/DeviceRGB)
    GFXDeviceRGB
  | -- | CMYK colour space (/DeviceCMYK)
    GFXDeviceCMYK
  | -- | CalGray colour space (/CalGray)
    GFXCIECalGray
  | -- | CalRGB colour space (/CalRGB)
    GFXCIECalRGB
  | -- | Lab colour space (/Lab)
    GFXCIELab
  | -- | ICC-based colour space (/ICCBased)
    GFXCIEICCBased
  | -- | Indexed colour space (/Indexed)
    GFXSpecialIndexed
  | -- | Pattern colour space (/Pattern)
    GFXSpecialPattern
  | -- | Separation colour space (/Separation)
    GFXSpecialSeparation
  | -- | DeviceN colour space (/DeviceN)
    GFXSpecialDeviceN
  deriving stock (Eq, Show)

-- | PDF operator
type GSOperator :: Type
data GSOperator
  = -- | Save the current graphics state on the graphics state stack (q)
    GSSaveGS
  | -- | Restore the graphics state (Q)
    GSRestoreGS
  | -- | Modify the current transformation matrix (cm)
    GSSetCTM
  | -- | Set the line width in the graphics state (w)
    GSSetLineWidth
  | -- | Set the line cap style in the graphics state (J)
    GSSetLineCap
  | -- | Set the line join style in the graphics state (j)
    GSSetLineJoin
  | -- | Set the miter limit in the graphics state (M)
    GSSetMiterLimit
  | -- | Set the line dash pattern in the graphics state (d)
    GSSetLineDashPattern
  | -- | Set the colour rendering intent in the graphics state (ri)
    GSSetColourRenderingIntent
  | -- | Set the flatness tolerance in the graphics state (i)
    GSSetFlatnessTolerance
  | -- | Set the specified parameters in the graphics state (gs)
    GSSetParameters
  | -- | Begin a new subpath by moving the current point to coordinates (m)
    GSMoveTo
  | -- | Append a straight line segment from the current point to the point (l)
    GSLineTo
  | -- | Append a cubic Bézier curve to the current path (c)
    GSCubicBezierCurve
  | -- | Append a cubic Bézier curve to the current path. (v)
    GSCubicBezierCurve1To
  | -- | Append a cubic Bézier curve to the current path. (y)
    GSCubicBezierCurve2To
  | -- | Close the current subpath by appending a straight line segment (h)
    GSCloseSubpath
  | -- | Append a rectangle to the current path as a complete subpath (re)
    GSRectangle
  | -- | Stroke the path (S)
    GSStrokePath
  | -- | Close and stroke the path (s)
    GSCloseStrokePath
  | -- | Fill the path, using the nonzero winding number rule (f)
    GSFillPathNZWR
  | -- | Fill the path, using the even-odd rule (f*)
    GSFillPathEOR
  | -- | Fill and then stroke the path, using the NZW rule (B)
    GSFillStrokePathNZWR
  | -- | Fill and then stroke the path, using the even-odd rule (B*)
    GSFillStrokePathEOR
  | -- | Close, fill, and then stroke the path, using the NZW rule (b)
    GSCloseFillStrokeNZWR
  | -- | Close, fill, and then stroke the path, using the even-odd rule (b*)
    GSCloseFillStrokeEOR
  | -- | End the path object without filling or stroking it (n)
    GSEndPath
  | -- | Begin a text object (BT)
    GSBeginText
  | -- | End a text object (ET)
    GSEndText
  | -- | Move to start of the next line (Td)
    GSMoveToNextLine
  | -- | Move to start of the next line while setting the leading parameter (TD)
    GSMoveToNextLineLP
  | -- | Set the text matrix (Tm)
    GSSetTextMatrix
  | -- | Move to the start of the next line (T*)
    GSNextLine
  | -- | Show a text string (Tj)
    GSShowText
  | -- | Move to the next line and show a text string (')
    GSNLShowText
  | -- | Move to the next line and show a text string with word spacing (")
    GSNLShowTextWithSpacing
  | -- | Show one or more text strings (TJ)
    GSShowManyText
  | -- | Set the character spacing (Tc)
    GSSetCharacterSpacing
  | -- | Set the word spacing (Tw)
    GSSetWordSpacing
  | -- | Set the horizontal scaling (Tz)
    GSSetHorizontalScaling
  | -- | Set the text leading (TL)
    GSSetTextLeading
  | -- | Set the text font (Tf)
    GSSetTextFont
  | -- | Set the text rendering mode (Tr)
    GSSetTextRenderingMode
  | -- | Set the text rise (Ts)
    GSSetTextRise
  | -- | Set width information for the glyph (d0)
    GSSetGlyphWidth
  | -- | Set width and bounding box information for the glyph (d1)
    GSSetBoundingBoxGlyph
  | -- | Set the current colour space to use for stroking operations (CS)
    GSSetStrokeColorspace
  | -- | Set the current colour space to use for nonstroking operations (cs)
    GSSetNonStrokeColorspace
  | -- | Set the colour to use for stroking operations in a device (SC)
    GSSetStrokeColor
  | -- | Set the colour to use for stroking operations in a deviceN (SCN)
    GSSetStrokeColorN
  | -- | Set the colour to use for nonstroking operations in a device (sc)
    GSSetNonStrokeColor
  | -- | Set the colour to use for nonstroking operations in a deviceN (scn)
    GSSetNonStrokeColorN
  | -- | Set the stroking colour space to DeviceGray (G)
    GSSetStrokeGrayColorspace
  | -- | Set the nonstroking colour space to DeviceGray (g)
    GSSetNonStrokeGrayColorspace
  | -- | Set the stroking colour space to DeviceRGB (RG)
    GSSetStrokeRGBColorspace
  | -- | Set the nonstroking colour space to DeviceRGB (rg)
    GSSetNonStrokeRGBColorspace
  | -- | Set the stroking colour space to DeviceCMYK (K)
    GSSetStrokeCMYKColorspace
  | -- | Set the nonstroking colour space to DeviceCMYK (k)
    GSSetNonStrokeCMYKColorspace
  | -- | Paint the shape and colour shading (sh)
    GSPaintShapeColourShading
  | -- | Begin an inline image object (BI)
    GSPaintXObject
  | -- | Designate a marked-content point (MP)
    GSMarkedContentPoint
  | -- | Designate a marked-content point with a property list (DP)
    GSMarkedContentPointPL
  | -- | Begin a marked-content sequence (BMC)
    GSBeginMarkedContentSequence
  | -- | Begin a marked-content sequence with a property list (BDC)
    GSBeginMarkedContentSequencePL
  | -- | End a marked-content sequence begun by a BMC or BDC operator (EMC)
    GSEndMarkedContentSequence
  | -- | Begin a compatibility section (BX)
    GSBeginCompatibilitySection
  | -- | End a compatibility section (EX)
    GSEndCompatibilitySection
  | -- | Modify current clipping path using the NZW rule (W)
    GSIntersectClippingPathNZWR
  | -- | Modify current clipping path using the even-odd rule (W*)
    GSIntersectClippingPathEOR
  | -- | Begin Inline image (BI)
    GSBeginInlineImage
  | -- | Inline image data (ID)
    GSInlineImageData
  | -- | End Inline image (EI)
    GSEndInlineImage
  | -- | Unknown operator
    GSUnknown !ByteString
  | -- | No-op operator
    GSNone
  deriving stock (Eq, Show)

{- |
Converts a `ByteString` to a `GSOperator`.

If the operator is unknown it is copied as is in a `GSUnknown`.
-}
toGSOperator :: ByteString -> GSOperator
toGSOperator "q"     = GSSaveGS
toGSOperator "Q"     = GSRestoreGS
toGSOperator "cm"    = GSSetCTM
toGSOperator "w"     = GSSetLineWidth
toGSOperator "J"     = GSSetLineCap
toGSOperator "j"     = GSSetLineJoin
toGSOperator "M"     = GSSetMiterLimit
toGSOperator "d"     = GSSetLineDashPattern
toGSOperator "ri"    = GSSetColourRenderingIntent
toGSOperator "i"     = GSSetFlatnessTolerance
toGSOperator "gs"    = GSSetParameters
toGSOperator "m"     = GSMoveTo
toGSOperator "l"     = GSLineTo
toGSOperator "c"     = GSCubicBezierCurve
toGSOperator "v"     = GSCubicBezierCurve1To
toGSOperator "y"     = GSCubicBezierCurve2To
toGSOperator "h"     = GSCloseSubpath
toGSOperator "re"    = GSRectangle
toGSOperator "S"     = GSStrokePath
toGSOperator "s"     = GSCloseStrokePath
toGSOperator "f"     = GSFillPathNZWR
toGSOperator "f*"    = GSFillPathEOR
toGSOperator "B"     = GSFillStrokePathNZWR
toGSOperator "B*"    = GSFillStrokePathEOR
toGSOperator "b"     = GSCloseFillStrokeNZWR
toGSOperator "b*"    = GSCloseFillStrokeEOR
toGSOperator "n"     = GSEndPath
toGSOperator "BT"    = GSBeginText
toGSOperator "ET"    = GSEndText
toGSOperator "Td"    = GSMoveToNextLine
toGSOperator "TD"    = GSMoveToNextLineLP
toGSOperator "Tm"    = GSSetTextMatrix
toGSOperator "T*"    = GSNextLine
toGSOperator "Tj"    = GSShowText
toGSOperator "'"     = GSNLShowText
toGSOperator "\""    = GSNLShowTextWithSpacing
toGSOperator "TJ"    = GSShowManyText
toGSOperator "Tc"    = GSSetCharacterSpacing
toGSOperator "Tw"    = GSSetWordSpacing
toGSOperator "Tz"    = GSSetHorizontalScaling
toGSOperator "TL"    = GSSetTextLeading
toGSOperator "Tf"    = GSSetTextFont
toGSOperator "Tr"    = GSSetTextRenderingMode
toGSOperator "Ts"    = GSSetTextRise
toGSOperator "d0"    = GSSetGlyphWidth
toGSOperator "d1"    = GSSetBoundingBoxGlyph
toGSOperator "CS"    = GSSetStrokeColorspace
toGSOperator "cs"    = GSSetNonStrokeColorspace
toGSOperator "SC"    = GSSetStrokeColor
toGSOperator "SCN"   = GSSetStrokeColorN
toGSOperator "sc"    = GSSetNonStrokeColor
toGSOperator "scn"   = GSSetNonStrokeColorN
toGSOperator "G"     = GSSetStrokeGrayColorspace
toGSOperator "g"     = GSSetNonStrokeGrayColorspace
toGSOperator "RG"    = GSSetStrokeRGBColorspace
toGSOperator "rg"    = GSSetNonStrokeRGBColorspace
toGSOperator "K"     = GSSetStrokeCMYKColorspace
toGSOperator "k"     = GSSetNonStrokeCMYKColorspace
toGSOperator "sh"    = GSPaintShapeColourShading
toGSOperator "Do"    = GSPaintXObject
toGSOperator "MP"    = GSMarkedContentPoint
toGSOperator "DP"    = GSMarkedContentPointPL
toGSOperator "BMC"   = GSBeginMarkedContentSequence
toGSOperator "BDC"   = GSBeginMarkedContentSequencePL
toGSOperator "EMC"   = GSEndMarkedContentSequence
toGSOperator "BX"    = GSBeginCompatibilitySection
toGSOperator "EX"    = GSEndCompatibilitySection
toGSOperator "W"     = GSIntersectClippingPathNZWR
toGSOperator "W*"    = GSIntersectClippingPathEOR
toGSOperator "BI"    = GSBeginInlineImage
toGSOperator "ID"    = GSInlineImageData
toGSOperator "EI"    = GSEndInlineImage
toGSOperator unknown = GSUnknown unknown

{- |
Converts a `GSOperator` to a `ByteString`.
-}
fromGSOperator :: GSOperator -> ByteString
fromGSOperator GSSaveGS                       = "q"
fromGSOperator GSRestoreGS                    = "Q"
fromGSOperator GSSetCTM                       = "cm"
fromGSOperator GSSetLineWidth                 = "w"
fromGSOperator GSSetLineCap                   = "J"
fromGSOperator GSSetLineJoin                  = "j"
fromGSOperator GSSetMiterLimit                = "M"
fromGSOperator GSSetLineDashPattern           = "d"
fromGSOperator GSSetColourRenderingIntent     = "ri"
fromGSOperator GSSetFlatnessTolerance         = "i"
fromGSOperator GSSetParameters                = "gs"
fromGSOperator GSMoveTo                       = "m"
fromGSOperator GSLineTo                       = "l"
fromGSOperator GSCubicBezierCurve             = "c"
fromGSOperator GSCubicBezierCurve1To          = "v"
fromGSOperator GSCubicBezierCurve2To          = "y"
fromGSOperator GSCloseSubpath                 = "h"
fromGSOperator GSRectangle                    = "re"
fromGSOperator GSStrokePath                   = "S"
fromGSOperator GSCloseStrokePath              = "s"
fromGSOperator GSFillPathNZWR                 = "f"
fromGSOperator GSFillPathEOR                  = "f*"
fromGSOperator GSFillStrokePathNZWR           = "B"
fromGSOperator GSFillStrokePathEOR            = "B*"
fromGSOperator GSCloseFillStrokeNZWR          = "b"
fromGSOperator GSCloseFillStrokeEOR           = "b*"
fromGSOperator GSEndPath                      = "n"
fromGSOperator GSBeginText                    = "BT"
fromGSOperator GSEndText                      = "ET"
fromGSOperator GSMoveToNextLine               = "Td"
fromGSOperator GSMoveToNextLineLP             = "TD"
fromGSOperator GSSetTextMatrix                = "Tm"
fromGSOperator GSNextLine                     = "T*"
fromGSOperator GSShowText                     = "Tj"
fromGSOperator GSNLShowText                   = "'"
fromGSOperator GSNLShowTextWithSpacing        = "\""
fromGSOperator GSShowManyText                 = "TJ"
fromGSOperator GSSetCharacterSpacing          = "Tc"
fromGSOperator GSSetWordSpacing               = "Tw"
fromGSOperator GSSetHorizontalScaling         = "Tz"
fromGSOperator GSSetTextLeading               = "TL"
fromGSOperator GSSetTextFont                  = "Tf"
fromGSOperator GSSetTextRenderingMode         = "Tr"
fromGSOperator GSSetTextRise                  = "Ts"
fromGSOperator GSSetGlyphWidth                = "d0"
fromGSOperator GSSetBoundingBoxGlyph          = "d1"
fromGSOperator GSSetStrokeColorspace          = "CS"
fromGSOperator GSSetNonStrokeColorspace       = "cs"
fromGSOperator GSSetStrokeColor               = "SC"
fromGSOperator GSSetStrokeColorN              = "SCN"
fromGSOperator GSSetNonStrokeColor            = "sc"
fromGSOperator GSSetNonStrokeColorN           = "scn"
fromGSOperator GSSetStrokeGrayColorspace      = "G"
fromGSOperator GSSetNonStrokeGrayColorspace   = "g"
fromGSOperator GSSetStrokeRGBColorspace       = "RG"
fromGSOperator GSSetNonStrokeRGBColorspace    = "rg"
fromGSOperator GSSetStrokeCMYKColorspace      = "K"
fromGSOperator GSSetNonStrokeCMYKColorspace   = "k"
fromGSOperator GSPaintShapeColourShading      = "sh"
fromGSOperator GSPaintXObject                 = "Do"
fromGSOperator GSMarkedContentPoint           = "MP"
fromGSOperator GSMarkedContentPointPL         = "DP"
fromGSOperator GSBeginMarkedContentSequence   = "BMC"
fromGSOperator GSBeginMarkedContentSequencePL = "BDC"
fromGSOperator GSEndMarkedContentSequence     = "EMC"
fromGSOperator GSBeginCompatibilitySection    = "BX"
fromGSOperator GSEndCompatibilitySection      = "EX"
fromGSOperator GSIntersectClippingPathNZWR    = "W"
fromGSOperator GSIntersectClippingPathEOR     = "W*"
fromGSOperator GSBeginInlineImage             = "BI"
fromGSOperator GSInlineImageData              = "ID"
fromGSOperator GSEndInlineImage               = "EI"
fromGSOperator (GSUnknown unknown)            = unknown
fromGSOperator GSNone                         = ""

{-|
A GFX is a collection of objects, here named GFX objects.

Values contained are decoded, meaning they no longer contain escape sequences.
-}
type GFXObject :: Type
data GFXObject
  = -- | A comment (without the starting %)
    GFXComment ByteString
  | -- | A number (always stored as a double)
    GFXNumber Double
  | -- | A name (starting with /)
    GFXName ByteString
  | -- | A string (unescaped and without parenthesis)
    GFXString ByteString
  | -- | An hexadeicmal string (without less-than/greater-than signs)
    GFXHexString ByteString
  | -- | A reference, number and generation (two integers followed by an `R`)
    GFXReference Int Int
  | -- | An array containing a list of objects
    GFXArray (Array GFXObject)
  | -- | A dictionary containing key-value pairs
    GFXDictionary (Dictionary GFXObject)
  | -- | A boolean (true or false)
    GFXBool Bool
  | -- | A null value
    GFXNull
  | -- | An inline image
    GFXInlineImage (Dictionary GFXObject) ByteString
  | -- | An operator
    GFXOperator GSOperator
  deriving stock (Eq, Show)

{- |
Create an empty `GFXDictionary`.
-}
mkEmptyGFXDictionary :: GFXObject
mkEmptyGFXDictionary = GFXDictionary mkEmptyDictionary

{- |
Create an empty `GFXArray`.
-}
mkEmptyGFXArray :: GFXObject
mkEmptyGFXArray = GFXArray mkEmptyArray

{- |
Create a `GFXDictionary` from a list of couples (key, value).
-}
mkGFXDictionary :: [(ByteString, GFXObject)] -> GFXObject
mkGFXDictionary = GFXDictionary . mkDictionary

{- |
Create a `GFXArray` from a list of `GFXObject`.
-}
mkGFXArray :: [GFXObject] -> GFXObject
mkGFXArray = GFXArray . mkArray

{- |
Indicates whether the `GFXObject` ends with a delimiter when converted to a
`ByteString`.
-}
endsWithDelimiter :: GFXObject -> Bool
endsWithDelimiter GFXComment{}         = True
endsWithDelimiter GFXNumber{}          = False
endsWithDelimiter GFXName{}            = False
endsWithDelimiter GFXString{}          = True
endsWithDelimiter GFXHexString{}       = True
endsWithDelimiter GFXReference{}       = False
endsWithDelimiter GFXArray{}           = True
endsWithDelimiter GFXDictionary{}      = True
endsWithDelimiter GFXBool{}            = False
endsWithDelimiter GFXNull              = False
endsWithDelimiter GFXInlineImage{}     = False
endsWithDelimiter (GFXOperator GSNone) = True
endsWithDelimiter GFXOperator{}        = False

{- |
Indicates whether the `GFXObject` starts with a delimiter when converted to a
`ByteString`.
-}
startsWithDelimiter :: GFXObject -> Bool
startsWithDelimiter GFXComment{}         = True
startsWithDelimiter GFXNumber{}          = False
startsWithDelimiter GFXName{}            = True
startsWithDelimiter GFXString{}          = True
startsWithDelimiter GFXHexString{}       = True
startsWithDelimiter GFXReference{}       = False
startsWithDelimiter GFXArray{}           = True
startsWithDelimiter GFXDictionary{}      = True
startsWithDelimiter GFXBool{}            = False
startsWithDelimiter GFXNull              = False
startsWithDelimiter GFXInlineImage{}     = False
startsWithDelimiter (GFXOperator GSNone) = True
startsWithDelimiter GFXOperator{}        = False

{- |
Tells if a space must be inserted between 2 `GFXObject` when converted to
`ByteString`.
-}
spaceIfNeeded :: GFXObject -> GFXObject -> ByteString
spaceIfNeeded object1 object2 | endsWithDelimiter object1   = ""
                              | startsWithDelimiter object2 = ""
                              | otherwise                   = " "

{- |
Converts a `GFXObject` to a `ByteString` ready to be inserted in a graphics
object in a stream.
-}
fromGFXObject :: GFXObject -> ByteString
fromGFXObject (GFXComment   comment  ) = BS.concat ["%", comment, "\n"]
fromGFXObject (GFXNumber    number   ) = fromNumber number
fromGFXObject (GFXName      name     ) = fromName name
fromGFXObject (GFXString    bytes    ) = fromString bytes
fromGFXObject (GFXHexString hexstring) = fromHexString hexstring
fromGFXObject (GFXReference number revision) =
  BS.concat [fromInt number, " ", fromInt revision, " R"]
fromGFXObject (GFXArray      objects   ) = fromArray objects
fromGFXObject (GFXDictionary dictionary) = fromDictionary dictionary
fromGFXObject (GFXBool       True      ) = "true"
fromGFXObject (GFXBool       False     ) = "false"
fromGFXObject GFXNull                    = "null"
fromGFXObject (GFXInlineImage dict raw)  = fromInlineImage dict raw
fromGFXObject (GFXOperator operator   )  = fromGSOperator operator

{- |
Returns a `Text` string describing a GFXObject.
-}
objectInfo :: GFXObject -> TL.Text
objectInfo (GFXComment comment) = format ("{- " % utf8 % " -}") comment
objectInfo (GFXNumber number) =
  format ("[number:" % utf8 % "]") (fromNumber number)
objectInfo (GFXName   name ) = format ("[name:" % utf8 % "]") name
objectInfo (GFXString bytes) = format ("[string:" % utf8 % "]") bytes
objectInfo (GFXHexString hexstring) =
  format ("[hexstring: " % utf8 % "]") hexstring
objectInfo (GFXReference number revision) =
  format ("[ref:" % int % "," % int % "]") number revision
objectInfo (GFXArray objects) =
  format ("[array:count=" % int % "]") (length objects)
objectInfo (GFXDictionary dictionary) =
  format ("[dictionary:count=" % int % "]") (Map.size dictionary)
objectInfo (GFXBool True )           = "true"
objectInfo (GFXBool False)           = "false"
objectInfo GFXNull                   = "null"
objectInfo (GFXInlineImage dict raw) = format
  ("[inlineimage:count=" % int % ", size=" % int % "]")
  (Map.size dict)
  (BS.length raw)
objectInfo GFXOperator{} = "operator"

{- |
Takes an `Array` of `GFXObject`, converts them to the `ByteString`
representation and inserts spaces between them if necessary.
-}
separateGfx :: Array GFXObject -> ByteString
separateGfx objects = BS.concat $ buildBS (filter notNull $ toList objects)
  where
    notNull :: GFXObject -> Bool
    notNull (GFXOperator GSNone) = False
    notNull _otherObject         = True

    buildBS :: [GFXObject] -> [ByteString]
    buildBS [] = []
    buildBS [object1] = [fromGFXObject object1]
    buildBS (object1:object2:others) = fromGFXObject object1
                                     : spaceIfNeeded object1 object2
                                     : buildBS (object2:others)

fromArray :: Array GFXObject -> ByteString
fromArray items = BS.concat ["[", separateGfx items, "]"]

fromDictionary :: Dictionary GFXObject -> ByteString
fromDictionary keyValues = BS.concat
  ["<<", separateGfx (splitCouple (mkArray (Map.toAscList keyValues))), ">>"]
 where
  splitCouple SQ.Empty = SQ.empty
  splitCouple ((key, value) SQ.:<| remains) =
    GFXName key SQ.:<| value SQ.:<| splitCouple remains

fromInlineImage :: Dictionary GFXObject -> ByteString -> ByteString
fromInlineImage keyValues image = BS.concat
  [ "BI "
  , separateGfx (splitCouple (mkArray (Map.toAscList keyValues)))
  , " ID\n"
  , image
  , "\nEI"
  ]
 where
  splitCouple SQ.Empty = SQ.empty
  splitCouple ((key, value) SQ.:<| remains) =
    GFXName (abbreviateKey key)
      SQ.:<| abbreviateValue value
      SQ.:<| splitCouple remains

  abbreviateValue :: GFXObject -> GFXObject
  abbreviateValue (GFXName "DeviceGray"     ) = GFXName "G"
  abbreviateValue (GFXName "DeviceRGB"      ) = GFXName "RGB"
  abbreviateValue (GFXName "DeviceCMYK"     ) = GFXName "CMYK"
  abbreviateValue (GFXName "Indexed"        ) = GFXName "I"
  abbreviateValue (GFXName "ASCIIHexDecode" ) = GFXName "AHx"
  abbreviateValue (GFXName "ASCII85Decode"  ) = GFXName "A85"
  abbreviateValue (GFXName "LZWDecode"      ) = GFXName "LZW"
  abbreviateValue (GFXName "FlateDecode"    ) = GFXName "Fl"
  abbreviateValue (GFXName "RunLengthDecode") = GFXName "RL"
  abbreviateValue (GFXName "CCITTFaxDecode" ) = GFXName "CCF"
  abbreviateValue (GFXName "DCTDecode"      ) = GFXName "DCT"
  abbreviateValue value                       = value

  abbreviateKey :: ByteString -> ByteString
  abbreviateKey "BitsPerComponent" = "BPC"
  abbreviateKey "ColorSpace"       = "CS"
  abbreviateKey "Decode"           = "D"
  abbreviateKey "DecodeParms"      = "DP"
  abbreviateKey "Filter"           = "F"
  abbreviateKey "Height"           = "H"
  abbreviateKey "ImageMask"        = "IM"
  abbreviateKey "Interpolate"      = "I"
  abbreviateKey "Width"            = "W"
  abbreviateKey key                = key

{- |
Round a GFXNumber or GFXNumbers in GFXArray to a given precision.

Any other GFXObject is returned as is.
-}
reducePrecision :: Int -> GFXObject -> GFXObject
reducePrecision precision (GFXNumber value) =
  GFXNumber (round' precision value)
reducePrecision precision (GFXArray items) =
  GFXArray (reducePrecision precision <$> items)
reducePrecision _anyPrecision gfxObject = gfxObject
