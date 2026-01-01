{-|
This module contains a parser for PDF start XREF object.

The last line of the file shall contain only the end-of-file marker, %%EOF.

The two preceding lines shall contain, one per line and in order, the keyword
startxref and the byte offset in the decoded stream from the beginning of the
file to the beginning of the xref keyword in the last cross-reference section.

The startxref line shall be preceded by the trailer dictionary.
-}
module PDF.Object.Parser.StartXRef
  ( startXRefP
  ) where

import Data.Binary.Parser (Get, isDigit, label, satisfy, some', string)

import PDF.Object.Object (PDFObject (PDFStartXRef))
import PDF.Object.Parser.LooseEndOfLine (looseEndOfLineP)

import Util.Number (toNumber)

{-|
Parse a `PDFXRef` object.
-}
startXRefP :: Get PDFObject
startXRefP = label "startxref" $ do
  string "startxref"
  looseEndOfLineP
  offset <- toNumber <$> some' (satisfy isDigit)
  looseEndOfLineP
  return $ PDFStartXRef offset
