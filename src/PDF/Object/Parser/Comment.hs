{-|
This module contains a parser for PDF comments.
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
A binary parser for the end-of-file comment `%EOF`.
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
A binary parser for a PDF comment.

A PDF comment is a line starting with `asciiPERCENTSIGN`.

It returns either:

- `PDFVersion` for any comment starting with `PDF-`
- `PDFComment` for any other string
 -}
commentP :: Get PDFObject
commentP = label "comment" $ do
  word8 asciiPERCENTSIGN
  comment <- takeTill isLooseEndOfLine
  endOfInput <|> looseEndOfLineP
  return $ case BS.splitAt 4 comment of
    ("PDF-", version) -> PDFVersion version
    (_     , _      ) -> PDFComment comment
