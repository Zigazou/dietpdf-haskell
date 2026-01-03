{-|
Parser for the startxref offset in PDF documents

This module provides a binary parser for the startxref section of PDF documents.

The startxref section appears near the end of a PDF file and contains the byte
offset of the last cross-reference section. The structure is:

1. The keyword "startxref"
2. One or more line terminators
3. A decimal offset value
4. One or more line terminators
5. (Followed by the %%EOF marker, parsed separately)

The offset value indicates the byte position from the beginning of the file to
the start of the xref keyword in the last cross-reference section.
-}
module PDF.Object.Parser.StartXRef
  ( startXRefP
  ) where

import Data.Binary.Parser (Get, isDigit, label, satisfy, some', string)

import PDF.Object.Object (PDFObject (PDFStartXRef))
import PDF.Object.Parser.LooseEndOfLine (looseEndOfLineP)

import Util.Number (toNumber)

{-|
Parse the startxref section of a PDF document.

Parses the keyword "startxref" followed by the byte offset of the last
cross-reference section. The parser handles line terminators (which may include
carriage returns, line feeds, or both) between components.

The offset value is a decimal integer representing the byte position from the
beginning of the decoded file to the "xref" keyword in the last cross-reference
section.

__Returns:__ A PDF startxref object containing the parsed byte offset value.

__Fails:__ If the keyword "startxref" is not found or the format is invalid.
-}
startXRefP :: Get PDFObject
startXRefP = label "startxref" $ do
  string "startxref"
  looseEndOfLineP
  offset <- toNumber <$> some' (satisfy isDigit)
  looseEndOfLineP
  return $ PDFStartXRef offset
