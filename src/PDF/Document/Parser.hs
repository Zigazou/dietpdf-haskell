{- |
Top-level PDF parsing entry point.

This module provides the top-level parser used to turn a raw PDF file
('ByteString') into a structured 'Data.PDF.PDFDocument.PDFDocument'.

Parsing is performed using the project's binary parser infrastructure
(`Data.Binary.Parser`) and errors are rewrapped into
'Data.UnifiedError.UnifiedError' so they can flow through the standard
error-handling stack.
-}
module PDF.Document.Parser
  ( pdfParse
  ) where

import Control.Applicative ((<|>))
import Control.Monad.Trans.Except (throwE)

import Data.Binary.Parser (Get, label, many', parseDetail, satisfy)
import Data.ByteString (ByteString)
import Data.Context (Context (Context))
import Data.Fallible (FallibleT)
import Data.Logging (Logging, sayF)
import Data.PDF.PDFDocument (PDFDocument, dSepBy)
import Data.PDF.PDFObject (PDFObject)
import Data.UnifiedError (UnifiedError (ParseError))
import Data.Word (Word8)

import PDF.Object.Object.PDFCharacter (isWhiteSpace)
import PDF.Object.Parser.Comment (commentP, eofP)
import PDF.Object.Parser.IndirectObject (indirectObjectP)
import PDF.Object.Parser.StartXRef (startXRefP)
import PDF.Object.Parser.Trailer (trailerP)
import PDF.Object.Parser.XRef (xrefP)

{- |
Parse a run of PDF whitespace characters.

This is used as a separator between top-level objects.
-}
whiteSpaces :: Get [Word8]
whiteSpaces = many' (satisfy isWhiteSpace)

{- |
Parse one top-level syntactic item.

At the document level, PDF files are modeled here as a sequence of indirect
objects, trailers, xref tables, comments, and the startxref marker.
-}
topObjectP :: Get PDFObject
topObjectP = eofP
         <|> commentP
         <|> indirectObjectP
         <|> trailerP
         <|> xrefP
         <|> startXRefP

{- |
Parse a whole PDF document as a sequence of top-level objects.
-}
pdfRawP :: Get PDFDocument
pdfRawP = label "pdf" $ topObjectP `dSepBy` whiteSpaces

{-|
Parses a PDF from a bytestring.

It encapsulates `parseDetail` in order to use the `UnifiedError` type when
returning errors.
-}
pdfParse
  :: Logging m
  => ByteString
  -> FallibleT m PDFDocument
pdfParse source = do
  sayF (Context "pdfParse") "Parsing PDF file"
  case parseDetail pdfRawP source of
    Left  err                      -> throwE (ParseError err)
    Right (""    , _     , result) -> return result
    Right (remain, offset, result) -> throwE
      (ParseError (remain, offset, "Stopped to read at offset " ++ show offset ++ show result))
