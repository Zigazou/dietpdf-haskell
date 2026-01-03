{-|
Parser for PDF comments and file markers

This module provides binary parsers for PDF comments and special comment
markers. Comments in PDF files start with a percent sign (%) and extend to the
end of the line. Special comments like %EOF and %PDF- are recognized and parsed
into dedicated object types.
-}
module PDF.Object.Parser.Comment
  ( commentP, eofP
  ) where

import Control.Applicative ((<|>))

import Data.Binary.Parser (Get, endOfInput, label, skipWhile, takeTill, word8)
import Data.ByteString qualified as BS

import PDF.Object.Object
  (PDFObject (PDFComment, PDFEndOfFile, PDFVersion), isWhiteSpace)
import PDF.Object.Parser.LooseEndOfLine (isLooseEndOfLine, looseEndOfLineP)

import Util.Ascii (asciiPERCENTSIGN, asciiUPPERE, asciiUPPERF, asciiUPPERO)

{-|
Parse the end-of-file marker in a PDF document.

The end-of-file marker is a special comment consisting of the five bytes @%EOF@.
It signals the end of a PDF file and may optionally be followed by whitespace.
After parsing, the parser consumes any remaining whitespace on the line.

Returns 'PDFEndOfFile' on successful parse.
-}
eofP :: Get PDFObject
eofP = label "eof" $ do
  word8 asciiPERCENTSIGN
  word8 asciiPERCENTSIGN
  word8 asciiUPPERE
  word8 asciiUPPERO
  word8 asciiUPPERF
  skipWhile isWhiteSpace
  return PDFEndOfFile

{-|
Parse a PDF comment from a PDF stream.

A PDF comment is a line starting with a percent sign (@%@) and extending to the
end of the line. The parser recognizes three special comment types:

1. @%EOF@ - The end-of-file marker (returns 'PDFEndOfFile')
2. @%PDF-version@ - The PDF version header (returns 'PDFVersion' with version
   string)
3. Any other comment - A regular comment (returns 'PDFComment' with content)

The parser consumes the percent sign prefix and any content until a line
terminator (CR, LF, or CR+LF), then optionally consumes the line terminator or
reaches end of input.

Returns a PDF object representing the parsed comment: 'PDFVersion',
'PDFEndOfFile', or 'PDFComment' depending on the comment content.
-}
commentP :: Get PDFObject
commentP = label "comment" $ do
  word8 asciiPERCENTSIGN
  comment <- takeTill isLooseEndOfLine
  endOfInput <|> looseEndOfLineP
  return $ case BS.splitAt 4 comment of
    ("%EOF", _      ) -> PDFEndOfFile
    ("PDF-", version) -> PDFVersion version
    (_     , _      ) -> PDFComment comment
