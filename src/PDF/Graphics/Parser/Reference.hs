{-|
Parser for PDF graphics stream object references

This module provides a binary parser for indirect object references in PDF
graphics streams.

A reference is an object value that allows one object to refer to another object
stored elsewhere in the PDF file. A reference has the form @n m R@ where @n@ is
the indirect object number, @m@ is its generation number, and @R@ is the
uppercase letter 'R' as a terminator.
-}
module PDF.Graphics.Parser.Reference
  ( referenceP
  ) where

import Data.Binary.Parser (Get, isDigit, label, satisfy, some', takeWhile1)
import Data.PDF.GFXObject (GFXObject (GFXReference), isKeywordCharacter)
import Data.Word (Word8)

import PDF.Graphics.Parser.EmptyContent (emptyContentP)

import Util.Number (toNumber)

{-|
Parse a single decimal digit (0-9).

Returns the byte value of the digit character.
-}
digit :: Get Word8
digit = satisfy isDigit

{-|
Parse one or more decimal digits as a list of byte values.

The parser requires at least one digit and returns all consecutive digits as a
list of their byte values (before numeric conversion).
-}
integerP :: Get [Word8]
integerP = some' digit

{-|
Parse an indirect object reference in a PDF graphics stream.

An indirect reference consists of:

1. An object number (one or more decimal digits)
2. Whitespace or empty content
3. A generation number (one or more decimal digits)
4. Whitespace or empty content
5. The uppercase letter 'R' as a terminator

The parser returns a graphics reference object containing the parsed object
number and generation number. The format matches the PDF specification: @n m R@
where @n@ is the object number, @m@ is the generation number, and @R@ is the
reference operator.

Example: @1 0 R@ parses to a reference to object 1, generation 0.
-}
referenceP :: Get GFXObject
referenceP = label "referenceG" $ do
  objectNumber <- toNumber <$> integerP
  emptyContentP
  revisionNumber <- toNumber <$> integerP
  emptyContentP
  keyword <- takeWhile1 isKeywordCharacter

  case keyword of
    "R" -> return $ GFXReference objectNumber revisionNumber
    _   -> fail "referenceP"
