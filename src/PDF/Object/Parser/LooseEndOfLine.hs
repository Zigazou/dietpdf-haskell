{-|
Parser for line terminators in PDF documents

This module provides a binary parser and predicate for line terminators in PDF
documents. PDF files support multiple line ending conventions: carriage return
(CR), line feed (LF), and the combination CR+LF. This module also handles
line terminators that are optionally preceded by whitespace.
-}
module PDF.Object.Parser.LooseEndOfLine
  ( looseEndOfLineP
  , isLooseEndOfLine
  ) where

import Control.Applicative ((<|>))

import Data.Binary.Parser (Get, word8)
import Data.Word (Word8)

import Util.Ascii
    ( asciiCR
    , asciiLF
    , asciiSPACE
    , pattern AsciiCR
    , pattern AsciiLF
    )

{-|
Test whether a byte represents a line terminator character.

Returns 'True' if the byte is a carriage return (CR, 0x0D) or line feed (LF,
0x0A), and 'False' for all other byte values.
-}
isLooseEndOfLine :: Word8 -> Bool
isLooseEndOfLine AsciiCR            = True
isLooseEndOfLine AsciiLF            = True
isLooseEndOfLine _anyOtherCharacter = False

{-|
Parse a line terminator in a PDF document, with optional leading whitespace.

This parser recognizes five possible line ending formats:

1. CR+LF (carriage return followed by line feed)
2. SPACE+CR (space followed by carriage return)
3. SPACE+LF (space followed by line feed)
4. CR (carriage return alone)
5. LF (line feed alone)

The parser tries each format in sequence with the space-prefixed variants first,
allowing for documents that have optional space before line terminators.
-}
looseEndOfLineP :: Get ()
looseEndOfLineP =
  (word8 asciiCR >> word8 asciiLF)
    <|> (word8 asciiSPACE >> word8 asciiCR)
    <|> (word8 asciiSPACE >> word8 asciiLF)
    <|> word8 asciiCR
    <|> word8 asciiLF
