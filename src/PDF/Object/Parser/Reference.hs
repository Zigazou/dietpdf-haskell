{-|
Parser for PDF object references

This module provides a binary parser for PDF object references.

A PDF reference is an indirect object reference that allows one object to refer
to another object in the document. It has the form "n m R" where:

- @n@ is the object number of the referenced indirect object
- @m@ is the generation number (version) of that object
- @R@ is the literal uppercase letter 'R' marking the reference

For example, @1 0 R@ refers to object number 1, generation 0.
-}
module PDF.Object.Parser.Reference
  ( referenceP
  ) where

import Data.Binary.Parser (Get, isDigit, label, satisfy, some', takeWhile1)
import Data.Word (Word8)

import PDF.Object.Object (PDFObject (PDFReference), isKeywordCharacter)
import PDF.Object.Parser.EmptyContent (emptyContentP)

import Util.Number (toNumber)

{-|
Parse a single decimal digit (0-9).

Returns the byte value of the digit character.
-}
digit :: Get Word8
digit = satisfy isDigit

{-|
Parse a sequence of decimal digits forming an integer.

Consumes one or more consecutive digit characters and returns them as a list of
byte values. This list can then be converted to a numeric value using utility
functions like 'toNumber'.

__Returns:__ A non-empty list of digit bytes.
-}
integerP :: Get [Word8]
integerP = some' digit

{-|
Parse a PDF object reference.

A reference consists of an object number, whitespace, a generation number,
whitespace, and the literal letter 'R'. The parser reads these components in
sequence and validates that the reference ends with 'R'.

The object and generation numbers are parsed as decimal integers and converted
to numeric values. Whitespace and comments between components are automatically
skipped by 'emptyContentP'.

Examples:

- @1 0 R@ references object 1, generation 0
- @42 5 R@ references object 42, generation 5
- @10  7  R@ handles multiple spaces between components

__Returns:__ A PDF reference object containing the object and generation
numbers, or fails if the reference format is invalid (e.g., missing 'R').
-}
referenceP :: Get PDFObject
referenceP = label "reference" $ do
  objectNumber <- toNumber <$> integerP
  emptyContentP
  revisionNumber <- toNumber <$> integerP
  emptyContentP
  keyword <- takeWhile1 isKeywordCharacter

  case keyword of
    "R" -> return $ PDFReference objectNumber revisionNumber
    _   -> fail "referenceP"
