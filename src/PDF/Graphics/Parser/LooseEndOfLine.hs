{-|
Parser for line terminators in PDF graphics streams

This module provides a binary parser and predicate for line terminators in PDF
graphics streams. PDF files support multiple line ending conventions: carriage
return (CR), line feed (LF), and the combination CR+LF.
-}
module PDF.Graphics.Parser.LooseEndOfLine
  ( looseEndOfLineP
  , isLooseEndOfLine
  ) where

import Control.Applicative ((<|>))

import Data.Binary.Parser (Get, word8)
import Data.Word (Word8)

import Util.Ascii (asciiCR, asciiLF, pattern AsciiCR, pattern AsciiLF)

{-|
Test whether a byte represents a line terminator.

Returns 'True' if the byte is a carriage return (CR) or line feed (LF), and
'False' for all other byte values.
-}
isLooseEndOfLine :: Word8 -> Bool
isLooseEndOfLine AsciiCR            = True
isLooseEndOfLine AsciiLF            = True
isLooseEndOfLine _anyOtherCharacter = False

{-|
Parse a line terminator in a PDF graphics stream.

This parser recognizes three possible line ending formats:

- CR+LF (carriage return followed by line feed)
- CR (carriage return alone)
- LF (line feed alone)

The parser tries CR+LF first, then falls back to CR or LF individually.
-}
looseEndOfLineP :: Get ()
looseEndOfLineP =
  (word8 asciiCR >> word8 asciiLF) <|> word8 asciiCR <|> word8 asciiLF
