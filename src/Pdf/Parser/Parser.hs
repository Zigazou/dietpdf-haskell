module Pdf.Parser.Parser
  ( pdfParse
  ) where

import           Control.Applicative            ( (<|>) )
import           Data.Binary.Parser             ( Get
                                                , label
                                                , many'
                                                , parseDetail
                                                , satisfy
                                                , sepBy
                                                )
import qualified Data.ByteString               as BS
import           Data.Word                      ( Word8 )
import           Pdf.Object.Object              ( PDFObject
                                                , isWhiteSpace
                                                )
import           Pdf.Parser.Comment             ( commentP )
import           Pdf.Parser.IndirectObject      ( indirectObjectP )
import           Pdf.Parser.StartXRef           ( startXRefP )
import           Pdf.Parser.Trailer             ( trailerP )
import           Pdf.Parser.XRef                ( xrefP )
import           Util.Errors                    ( UnifiedError(ParseError) )

whiteSpaces :: Get [Word8]
whiteSpaces = many' (satisfy isWhiteSpace)

topObjectP :: Get PDFObject
topObjectP = commentP <|> indirectObjectP <|> trailerP <|> xrefP <|> startXRefP

pdfRawP :: Get [PDFObject]
pdfRawP = label "pdf" $ topObjectP `sepBy` whiteSpaces

{-|
Parses a PDF from a bytestring.

It encapsulates `parseDetail` in order to use the `UnifiedError` type when
returning errors.
-}
pdfParse
  :: BS.ByteString -- ^ The bytestring to parse coming from a file.
  -> Either UnifiedError [PDFObject] -- ^ Error or a list of PDF objects.
pdfParse source = case parseDetail pdfRawP source of
  Left  err            -> Left (ParseError err)
  Right (_, _, result) -> Right result
