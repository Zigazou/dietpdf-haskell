{-|
Parser for PDF trailer dictionaries

This module provides a binary parser for PDF trailer dictionaries.

The trailer of a PDF file enables a conforming reader to quickly locate the
cross-reference table and certain special objects. Conforming readers should
read a PDF file from its end.

The trailer structure near the end of a PDF file is:

1. The keyword "trailer"
2. Optional whitespace
3. A dictionary enclosed in double angle brackets (@<< ... >>@) containing
   key-value pairs such as Size, Root, Encrypt, Info, ID, and Prev

The trailer dictionary is followed by the startxref section, which contains the
byte offset to the last cross-reference section.
-}
module PDF.Object.Parser.Trailer
  ( trailerP
  ) where

import Data.Binary.Parser (Get, label, skipWhile, string)

import PDF.Object.Object (PDFObject (PDFTrailer), isWhiteSpace)
import PDF.Object.Parser.Container (dictionaryP)

{-|
Parse a PDF trailer dictionary.

The trailer is a special dictionary that follows the cross-reference table and
provides the reader with information needed to locate and interpret the contents
of the file. It begins with the keyword "trailer" followed by a dictionary.

The parser skips any whitespace after the keyword and then parses the dictionary
containing key-value pairs such as:

- @Size@: Total number of objects in the file
- @Root@: Reference to the document catalog
- @Encrypt@: (Optional) Reference to encryption dictionary
- @Info@: (Optional) Reference to document information dictionary
- @ID@: (Optional) File identifier
- @Prev@: (Optional) Offset to previous trailer (for incremental updates)

__Returns:__ A PDF trailer object containing the parsed dictionary.

__Fails:__ If the keyword "trailer" is not found or the dictionary format is
invalid.
-}
trailerP :: Get PDFObject
trailerP = label "trailer" $ do
  string "trailer"
  skipWhile isWhiteSpace
  PDFTrailer <$> dictionaryP
