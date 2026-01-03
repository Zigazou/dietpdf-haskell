{-|
Parser for PDF string objects

This module provides a binary parser for string objects in PDF documents.

A string consists of a series of bytes (unsigned integer values in the range 0
to 255) stored in a compact form within parentheses.

Strings in PDF may contain escape sequences:

- @\\n@ for line feed (LF)
- @\\r@ for carriage return (CR)
- @\\t@ for horizontal tab (HT)
- @\\b@ for backspace (BS)
- @\\f@ for form feed (FF)
- @\\(@ for left parenthesis
- @\\)@ for right parenthesis
- @\\\\@ for reverse solidus (backslash)
- @\\ddd@ for octal character codes (1-3 digits)
- @\\@ at end of line escapes the line terminator

Text strings (as opposed to binary strings) intended to be human-readable may be
encoded in Unicode. Unicode strings are prefixed with the byte order marker
U+FEFF (bytes FE FF), indicating UTF-16BE (big-endian) encoding.
-}
module PDF.Object.Parser.String
  ( stringP
  ) where

import Control.Applicative ((<|>))

import Data.Binary.Parser (Get, label, many', satisfy, some', word8)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Maybe (catMaybes)
import Data.Word (Word8)

import PDF.Object.Object
    ( PDFObject (PDFString)
    , isOctal
    , isStringEscapeSequence
    , isStringRegularChar
    )
import PDF.Object.Parser.LooseEndOfLine (looseEndOfLineP)

import Util.Ascii
    ( asciiBS
    , asciiCR
    , asciiDIGITZERO
    , asciiFF
    , asciiHT
    , asciiLEFTPARENTHESIS
    , asciiLF
    , asciiREVERSESOLIDUS
    , asciiRIGHTPARENTHESIS
    , pattern AsciiLEFTPARENTHESIS
    , pattern AsciiLOWERB
    , pattern AsciiLOWERF
    , pattern AsciiLOWERN
    , pattern AsciiLOWERR
    , pattern AsciiLOWERT
    , pattern AsciiREVERSESOLIDUS
    , pattern AsciiRIGHTPARENTHESIS
    )

{-|
Parse a backslash followed by a line terminator within a string.

When a backslash appears immediately before a line terminator (CR, LF, or
CR+LF), it escapes the line terminator, meaning the line break is not included
in the resulting string. This allows strings to span multiple lines without
including newline characters.

__Returns:__ 'Nothing' to indicate the line terminator was consumed but
contributes no character to the output.
-}
escapedEndOfLineP :: Get (Maybe Word8)
escapedEndOfLineP =
  word8 asciiREVERSESOLIDUS >> looseEndOfLineP >> return Nothing

{-|
Parse a backslash followed by an escape character code.

Supported escape sequences:

- @\\n@ → line feed (ASCII 10)
- @\\r@ → carriage return (ASCII 13)
- @\\t@ → horizontal tab (ASCII 9)
- @\\b@ → backspace (ASCII 8)
- @\\f@ → form feed (ASCII 12)
- @\\(@ → left parenthesis (ASCII 40)
- @\\)@ → right parenthesis (ASCII 41)
- @\\\\@ → backslash (ASCII 92)

__Returns:__ 'Just' the byte value of the escaped character, or 'Nothing' if the
escape character is not recognized.

__Fails:__ If the character after the backslash is not a recognized escape
character.
-}
escapedCharP :: Get (Maybe Word8)
escapedCharP = do
  word8 asciiREVERSESOLIDUS
  satisfy isStringEscapeSequence >>= convert
 where
  convert :: Word8 -> Get (Maybe Word8)
  convert AsciiLOWERN           = return (Just asciiLF)
  convert AsciiLOWERR           = return (Just asciiCR)
  convert AsciiLOWERT           = return (Just asciiHT)
  convert AsciiLOWERB           = return (Just asciiBS)
  convert AsciiLOWERF           = return (Just asciiFF)
  convert AsciiLEFTPARENTHESIS  = return (Just asciiLEFTPARENTHESIS)
  convert AsciiRIGHTPARENTHESIS = return (Just asciiRIGHTPARENTHESIS)
  convert AsciiREVERSESOLIDUS   = return (Just asciiREVERSESOLIDUS)
  convert _anyOtherCharacter    = fail "escapedChar"

{-|
Parse a backslash followed by one to three octal digits.

Octal escape sequences allow specification of arbitrary byte values as
three-digit octal numbers (e.g., @\\377@), two-digit octal numbers (e.g.,
@\\77@), or single-digit octal numbers (e.g., @\\7@).

The parser attempts to match three digits first, then two, then one, returning
the byte value represented by the octal digits.

For example:

- @\\101@ (octal 101) → 65 (ASCII 'A')
- @\\50@ (octal 50) → 40 (ASCII '(')
- @\\0@ (octal 0) → 0

__Returns:__ 'Just' the byte value of the octal escape sequence.
-}
escapedOctalP :: Get (Maybe Word8)
escapedOctalP =
  word8 asciiREVERSESOLIDUS >> threeOctal <|> twoOctal <|> oneOctal
 where
  digitToNumber x = x - asciiDIGITZERO
  threeOctal = do
    first  <- digitToNumber <$> satisfy isOctal
    second <- digitToNumber <$> satisfy isOctal
    third  <- digitToNumber <$> satisfy isOctal
    return $ Just (first * 64 + second * 8 + third)

  twoOctal = do
    first  <- digitToNumber <$> satisfy isOctal
    second <- digitToNumber <$> satisfy isOctal
    return $ Just (first * 8 + second)

  oneOctal = Just . digitToNumber <$> satisfy isOctal

{-|
Drop the last @n@ bytes from a bytestring.

If @n@ is less than or equal to 0, returns the bytestring unchanged. If @n@ is
greater than or equal to the length of the bytestring, returns an empty
bytestring.

__Parameters:__

- The number of bytes to drop from the end
- The bytestring to process

__Returns:__ The bytestring with the last @n@ bytes removed, or empty if the
removal would consume the entire string.
-}
dropEnd :: Int -> ByteString -> ByteString
dropEnd n ps | n <= 0            = ps
             | n >= BS.length ps = ""
             | otherwise         = BS.take (BS.length ps - n) ps

{-|
Parse a single character, handling escape sequences.

This parser attempts to parse escape sequences in the following order:

1. Escaped line terminators (backslash before line break)
2. Named escape characters (@\\n@, @\\r@, @\\t@, etc.)
3. Octal escape sequences (@\\ddd@)
4. Regular (non-special) characters

__Returns:__ 'Just' the byte value of the parsed character, or 'Nothing' if the
character was an escaped line terminator.
-}
charP :: Get (Maybe Word8)
charP =
  escapedEndOfLineP
    <|> escapedCharP
    <|> escapedOctalP
    <|> (Just <$> satisfy isStringRegularChar)

{-|
Parse a sequence of characters with escape handling.

Parses one or more characters (using 'charP') and collects them into a
bytestring. Uses 'catMaybes' to filter out any 'Nothing' values from escaped
line terminators, which do not contribute bytes to the result.

__Returns:__ A bytestring containing the parsed bytes.
-}
charsP :: Get ByteString
charsP = BS.pack . catMaybes <$> some' charP

{-|
Parse a raw PDF string including delimiters and nested parentheses.

A PDF string is delimited by parentheses and may contain nested parentheses. The
parser handles nesting by recursively parsing inner strings. Returns the string
content including the outer parentheses, as these will be removed later by
'stringP'.

For example, @(Hello (world))@ will parse to include both sets of parentheses.

__Returns:__ The complete string including delimiters and any nested content.
-}
rawStringP :: Get ByteString
rawStringP = do
  word8 asciiLEFTPARENTHESIS
  content <- many' (rawStringP <|> charsP)
  word8 asciiRIGHTPARENTHESIS
  return $ BS.concat ["(", BS.concat content, ")"]

{-|
Parse a PDF string object.

A string consists of a series of bytes enclosed in parentheses. The parser
handles escape sequences within the string and removes the outer parentheses
from the result.

The resulting bytestring contains the processed string content with escape
sequences already decoded. For example, @(Hello\\nWorld)@ parses to the bytes
for "Hello" followed by a line feed, followed by "World".

__Returns:__ A PDF string object containing the parsed and processed byte
sequence.
-}
stringP :: Get PDFObject
stringP = label "string" (PDFString . BS.drop 1 . dropEnd 1 <$> rawStringP)
