module Pdf.Document.Parser
  ( pdfParse
  ) where

import Control.Applicative ((<|>))
import Control.Monad.Trans.Except (throwE)

import Data.Binary.Parser (Get, label, many', parseDetail, satisfy)
import Data.ByteString qualified as BS
import Data.Word (Word8)

import Pdf.Document.Document (PDFDocument, dSepBy)
import Pdf.Object.Object (PDFObject, isWhiteSpace)
import Pdf.Object.Parser.Comment (commentP)
import Pdf.Object.Parser.IndirectObject (indirectObjectP)
import Pdf.Object.Parser.StartXRef (startXRefP)
import Pdf.Object.Parser.Trailer (trailerP)
import Pdf.Object.Parser.XRef (xrefP)

import Util.Logging (Logging, sayF)
import Util.UnifiedError (FallibleT, UnifiedError (ParseError))

whiteSpaces :: Get [Word8]
whiteSpaces = many' (satisfy isWhiteSpace)

topObjectP :: Get PDFObject
topObjectP = commentP <|> indirectObjectP <|> trailerP <|> xrefP <|> startXRefP

pdfRawP :: Get PDFDocument
pdfRawP = label "pdf" $ topObjectP `dSepBy` whiteSpaces

{-|
Parses a PDF from a bytestring.

It encapsulates `parseDetail` in order to use the `UnifiedError` type when
returning errors.
-}
pdfParse
  :: Logging m
  => BS.ByteString -- ^ The bytestring to parse coming from a file.
  -> FallibleT m PDFDocument -- ^ Error or a `PDFDocument`.
pdfParse source = do
  sayF "Parsing PDF file"
  case parseDetail pdfRawP source of
    Left  err                      -> throwE (ParseError err)
    Right (""    , _     , result) -> return result
    Right (remain, offset, _     ) -> throwE
      (ParseError (remain, offset, "Stopped to read at offset " ++ show offset))
