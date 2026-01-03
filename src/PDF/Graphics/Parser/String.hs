{-|
Parser for PDF graphics stream string objects

This module provides a binary parser for string objects in PDF graphics streams.

A string consists of a series of bytes (unsigned integer values in the range
0 to 255) and the bytes are not integer objects, but are stored in a more
compact form.

The text string type shall be used for character strings that contain
information intended to be human-readable, such as text annotations, bookmark
names, article names, document information, and so forth.

For text strings encoded in Unicode, the first two bytes shall be 254 followed
by 255. These two bytes represent the Unicode byte order marker, U+FEFF,
indicating that the string is encoded in the UTF-16BE (big-endian) encoding
scheme specified in the Unicode standard.
-}
module PDF.Graphics.Parser.String
  ( stringP
  ) where

import Control.Applicative ((<|>))

import Data.Binary.Parser (Get, label, many', satisfy, some', word8)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Maybe (catMaybes)
import Data.PDF.GFXObject
    ( GFXObject (GFXString)
    , isOctal
    , isStringEscapeSequence
    , isStringRegularChar
    )
import Data.Word (Word8)

import PDF.Graphics.Parser.LooseEndOfLine (looseEndOfLineP)

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
Parse an escaped line terminator in a string.

When a backslash (reverse solidus) appears immediately before a line terminator
(CR, LF, or CR+LF), the entire sequence is treated as an escape sequence that
produces no output. This allows long strings to be split across multiple lines
in the PDF source without including the line break in the string content.

Returns 'Nothing' to indicate this sequence produces no character.
-}
escapedEndOfLineP :: Get (Maybe Word8)
escapedEndOfLineP =
  word8 asciiREVERSESOLIDUS >> looseEndOfLineP >> return Nothing

{-|
Parse an escaped special character in a string.

A backslash (reverse solidus) followed by a special character code represents
escaped characters:

- @\\n@ produces a line feed (LF, 0x0A)
- @\\r@ produces a carriage return (CR, 0x0D)
- @\\t@ produces a horizontal tab (HT, 0x09)
- @\\b@ produces a backspace (BS, 0x08)
- @\\f@ produces a form feed (FF, 0x0C)
- @\\(@ produces a left parenthesis
- @\\)@ produces a right parenthesis
- @\\\\@ produces a backslash

Returns 'Just' with the decoded byte, or fails if the character is not a
recognized escape code.
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
  convert _anyOtherCharacter    = fail "escapedCharG"

{-|
Parse an octal escape sequence in a string.

A backslash (reverse solidus) followed by one to three octal digits (0-7)
represents a byte value in octal notation. The parser accepts one, two, or three
octal digits after the backslash.

Examples:

- @\\101@ decodes to 0x41 ('A' in ASCII)
- @\\12@ decodes to 0x0A (line feed)
- @\\0@ decodes to 0x00 (null character)

Returns 'Just' with the decoded byte value.
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
Remove a specified number of bytes from the end of a bytestring.

If the number of bytes to remove is less than or equal to zero, the bytestring
is returned unchanged. If the number of bytes to remove is greater than or equal
to the length of the bytestring, an empty bytestring is returned.

__Parameters:__

- The number of bytes to remove from the end
- The bytestring to process

__Returns:__ A bytestring with the specified number of bytes removed from the
end.
-}
dropEnd :: Int -> ByteString -> ByteString
dropEnd n ps | n <= 0            = ps
             | n >= BS.length ps = ""
             | otherwise         = BS.take (BS.length ps - n) ps

{-|
Parse a single character in a string, handling escapes and regular characters.

A character in a string may be:

- An escaped line terminator (returns 'Nothing')
- An escaped special character (returns 'Just' with the decoded byte)
- An escaped octal sequence (returns 'Just' with the decoded byte)
- A regular string character (returns 'Just' with the byte)

The parser tries escape sequences first, then regular characters. Returns
'Maybe' 'Word8' to allow escape sequences that produce no output.
-}
charP :: Get (Maybe Word8)
charP =
  escapedEndOfLineP
    <|> escapedCharP
    <|> escapedOctalP
    <|> (Just <$> satisfy isStringRegularChar)

{-|
Parse a sequence of one or more string characters.

This parser accumulates characters (handling escapes appropriately) and packages
them into a bytestring. All 'Nothing' values from escape sequences that produce
no output are filtered out using 'catMaybes'.

Returns a bytestring containing all decoded character bytes.
-}
charsP :: Get ByteString
charsP = BS.pack . catMaybes <$> some' charP

{-|
Parse a raw string including nested parentheses.

A raw string is delimited by parentheses and may contain nested parentheses via
recursive string parsing. The parser handles balanced parentheses by recursively
parsing nested strings. The output includes the surrounding parentheses and all
nested content.

This lower-level parser is used by 'stringP' to extract the content between the
outermost parentheses.

Returns a bytestring containing the complete raw string including its
delimiters.
-}
rawStringP :: Get ByteString
rawStringP = label "rawStringG" $ do
  word8 asciiLEFTPARENTHESIS
  content <- many' (rawStringP <|> charsP)
  word8 asciiRIGHTPARENTHESIS
  return $ BS.concat ["(", BS.concat content, ")"]

{-|
Parse a string object from a PDF graphics stream.

A string is delimited by parentheses and may contain:

- Regular characters (letters, digits, punctuation)
- Escape sequences for special characters (@\\n@, @\\r@, @\\t@, etc.)
- Octal escape sequences (@\\101@, @\\12@, etc.)
- Escaped line terminators (backslash followed by line break)
- Nested parentheses (via recursive parsing with proper balancing)

The parser extracts the string content by removing the outer parentheses and
returns a graphics string object containing the decoded content.

Returns a graphics string object with the fully decoded and unescaped content.
-}
stringP :: Get GFXObject
stringP = label "stringG" (GFXString . BS.drop 1 . dropEnd 1 <$> rawStringP)
