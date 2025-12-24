{- |
This module provides a parser for PDF trailers.

The trailer of a PDF file enables a conforming reader to quickly find the
cross-reference table and certain special objects.

Conforming readers should read a PDF file from its end.

The last line of the file shall contain only the end-of-file marker, %%EOF.

The two preceding lines shall contain, one per line and in order, the keyword
startxref and the byte offset in the decoded stream from the beginning of the
file to the beginning of the xref keyword in the last cross-reference section.

The trailer dictionary shall precede the startxref line, consisting of the
keyword trailer followed by a series of key-value pairs enclosed in double
angle brackets (<< … >>) (using LESS-THAN SIGNs (3Ch) and GREATER-THAN SIGNs
(3Eh)).
-}
module PDF.Object.Parser.Trailer
  ( trailerP
  ) where

import Data.Binary.Parser (Get, label, skipWhile, string)

import PDF.Object.Object (PDFObject (PDFTrailer), isWhiteSpace)
import PDF.Object.Parser.Container (dictionaryP)

{- |
Parse a `PDFTrailer`.
-}
trailerP :: Get PDFObject
trailerP = label "trailer" $ do
  string "trailer"
  skipWhile isWhiteSpace
  PDFTrailer <$> dictionaryP
