{-|
This module contains a parser for PDF comments.
-}
module Pdf.Object.Parser.Comment
  ( commentP
  ) where

import Control.Applicative ((<|>))

import Data.Binary.Parser (Get, endOfInput, label, takeTill, word8)
import Data.ByteString qualified as BS

import Pdf.Object.Object (PDFObject (PDFComment, PDFEndOfFile, PDFVersion))
import Pdf.Object.Parser.LooseEndOfLine (isLooseEndOfLine, looseEndOfLineP)

import Util.Ascii (asciiPERCENTSIGN)

{-|
A binary parser for a PDF comment.

A PDF comment is a line starting with `asciiPERCENTSIGN`.

It returns either:

- `PDFEndOfFile` for any comment containing only `%EOF'
- `PDFVersion` for any comment starting with `PDF-`
- `PDFComment` for any other string
 -}
commentP :: Get PDFObject
commentP = label "comment" $ do
  word8 asciiPERCENTSIGN
  comment <- takeTill isLooseEndOfLine
  endOfInput <|> looseEndOfLineP
  return $ case BS.splitAt 4 comment of
    ("%EOF", ""     ) -> PDFEndOfFile
    ("PDF-", version) -> PDFVersion version
    (_     , _      ) -> PDFComment comment
