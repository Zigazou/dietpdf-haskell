module PDF.Document.Parser
  ( pdfParse
  ) where

import Control.Applicative ((<|>))
import Control.Monad.Trans.Except (throwE)

import Data.Binary.Parser (Get, label, many', parseDetail, satisfy)
import Data.ByteString (ByteString)
import Data.Context (Context (NoContext))
import Data.Fallible (FallibleT)
import Data.Logging (Logging, sayF)
import Data.PDF.PDFDocument (PDFDocument, dSepBy)
import Data.PDF.PDFObject (PDFObject)
import Data.UnifiedError (UnifiedError (ParseError))
import Data.Word (Word8)

import PDF.Object.Object.PDFCharacter (isWhiteSpace)
import PDF.Object.Parser.Comment (commentP)
import PDF.Object.Parser.IndirectObject (indirectObjectP)
import PDF.Object.Parser.StartXRef (startXRefP)
import PDF.Object.Parser.Trailer (trailerP)
import PDF.Object.Parser.XRef (xrefP)

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
  => ByteString -- ^ The bytestring to parse coming from a file.
  -> FallibleT m PDFDocument -- ^ Error or a `PDFDocument`.
pdfParse source = do
  sayF NoContext "Parsing PDF file"
  case parseDetail pdfRawP source of
    Left  err                      -> throwE (ParseError err)
    Right (""    , _     , result) -> return result
    Right (remain, offset, _     ) -> throwE
      (ParseError (remain, offset, "Stopped to read at offset " ++ show offset))
