{-|
This module contains a parser for PDF comments.
-}
module Pdf.Object.Parser.Comment
  ( commentP
  ) where

import           Data.Binary.Parser             ( Get
                                                , label
                                                , takeTill
                                                , word8, endOfInput
                                                )
import qualified Data.ByteString               as BS
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFComment
                                                  , PDFEndOfFile
                                                  , PDFVersion
                                                  )
                                                )
import           Util.Ascii                     ( asciiPERCENTSIGN )
import           Pdf.Object.Parser.LooseEndOfLine
                                                ( looseEndOfLineP
                                                , isLooseEndOfLine
                                                )
import Control.Applicative ((<|>))

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
