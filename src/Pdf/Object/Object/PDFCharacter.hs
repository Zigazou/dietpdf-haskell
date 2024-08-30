module Pdf.Object.Object.PDFCharacter
  ( -- * PDF characters
    isDelimiter
  , isPlusMinus
  , isWhiteSpace
  , isSpace
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
    ( asciiCR
    , asciiDIGITSEVEN
    , asciiDIGITZERO
    , asciiFF
    , asciiHT
    , asciiLF
    , asciiLOWERA
    , asciiLOWERZ
    , asciiNUL
    , asciiNUMBERSIGN
    , asciiSPACE
    , asciiUPPERA
    , asciiUPPERZ
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
isWhiteSpace byte = byte == asciiSPACE
                 || byte == asciiLF
                 || byte == asciiCR
                 || byte == asciiHT
                 || byte == asciiNUL
                 || byte == asciiFF

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
