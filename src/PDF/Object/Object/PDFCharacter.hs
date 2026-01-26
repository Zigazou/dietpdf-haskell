{-|
Character classification predicates for PDF syntax

This module provides predicate functions for classifying bytes according to PDF
syntax rules. These functions are used by parsers and serializers to validate
and process various PDF data structures.
-}
module PDF.Object.Object.PDFCharacter
  ( -- * PDF characters
    isDelimiter
  , isPlusMinus
  , isWhiteSpace
  , isKeywordCharacter
  , isOctal
  , isStringEscapeSequence
  , isStringRegularChar
  , isNameRegularChar
  ) where

import Data.ByteString qualified as BS
import Data.Ix (inRange)
import Data.Maybe (isJust, isNothing)
import Data.Word (Word8)

import Util.Ascii
    ( asciiDIGITSEVEN
    , asciiDIGITZERO
    , asciiLOWERA
    , asciiLOWERZ
    , asciiUPPERA
    , asciiUPPERZ
    , pattern AsciiCR
    , pattern AsciiFF
    , pattern AsciiHT
    , pattern AsciiLF
    , pattern AsciiNUL
    , pattern AsciiNUMBERSIGN
    , pattern AsciiSPACE
    )

{-|
Test whether a byte is a PDF delimiter character.

The following characters are considered delimiters: @(@, @)@, @<@, @>@, @[@,
@]@, @{@, @}@, @/@, and @%@.
-}
isDelimiter :: Word8 -> Bool
isDelimiter = isJust . flip BS.elemIndex "()<>[]{}/%"

{-|
Test whether a byte is a plus or minus sign.

Returns 'True' for @+@ (plus) and @-@ (minus) characters, 'False' otherwise.
-}
isPlusMinus :: Word8 -> Bool
isPlusMinus = isJust . flip BS.elemIndex "-+"

{-|
Test whether a byte is a whitespace character according to PDF specifications.

The following characters are considered whitespace:

- NUL (null, 0x00)
- HT (horizontal tab, 0x09)
- LF (line feed, 0x0A)
- FF (form feed, 0x0C)
- CR (carriage return, 0x0D)
- SP (space, 0x20)
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
Test whether a byte is a valid keyword character.

Keyword characters are alphabetical characters only: A-Z or a-z.
-}
isKeywordCharacter :: Word8 -> Bool
isKeywordCharacter byte = inRange (asciiLOWERA, asciiLOWERZ) byte
                       || inRange (asciiUPPERA, asciiUPPERZ) byte

{-|
Test whether a byte is an octal digit.

The following characters are considered octal digits: @0@, @1@, @2@, @3@,
@4@, @5@, @6@, and @7@.
-}
isOctal :: Word8 -> Bool
isOctal = inRange (asciiDIGITZERO, asciiDIGITSEVEN)

{-|
Test whether a byte is a valid escape sequence character in a PDF string.

The following characters are considered valid escape codes: @n@ (line feed), @r@
(carriage return), @t@ (tab), @b@ (backspace), @f@ (form feed), @(@ (left
parenthesis), @)@ (right parenthesis), and @\\@ (backslash).
-}
isStringEscapeSequence :: Word8 -> Bool
isStringEscapeSequence = isJust . flip BS.elemIndex "nrtbf()\\"

{-|
Test whether a byte is a valid regular (non-escaped) character in a PDF string.

Any character except @(@, @)@, and @\\@ (backslash) is considered a regular
string character.
-}
isStringRegularChar :: Word8 -> Bool
isStringRegularChar = isNothing . flip BS.elemIndex "()\\"

{-|
Test whether a byte is a valid character for a PDF name object.

A PDF name may contain any character except:

- NUL (null character, 0x00)
- Whitespace characters
- Delimiter characters (@()<>[]{}\/%@)
- NUMBER SIGN (@#@), which is reserved for hexadecimal escape encoding
-}
isNameRegularChar :: Word8 -> Bool
isNameRegularChar AsciiNUMBERSIGN          = False
isNameRegularChar AsciiNUL                 = False
isNameRegularChar byte | isWhiteSpace byte = False
                       | isDelimiter byte  = False
                       | otherwise         = True
