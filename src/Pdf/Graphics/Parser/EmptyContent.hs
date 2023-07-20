{- |
This module contains parser for empty contents (space or comment) used to
separate PDF objects.
-}
module Pdf.Graphics.Parser.EmptyContent
  ( emptyContentP
  ) where

import           Control.Applicative            ( (<|>) )
import           Data.Binary.Parser             ( Get
                                                , label
                                                , satisfy
                                                , skipMany
                                                )
import           Pdf.Object.Object              ( isWhiteSpace )
import           Pdf.Graphics.Parser.Comment    ( commentP )
import           Control.Monad                  ( void )

{-|
A binary parser meant to ignore empty content in a PDF file.

Empty content is either:

- white spaces (according to PDF specifications)
- comments (starting with a percent sign "%" and ending with a line return)
-}
emptyContentP :: Get ()
emptyContentP = label "emptycontentG"
  $ skipMany (void (satisfy isWhiteSpace) <|> void commentP)
