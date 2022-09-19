{- |
This module contains parser for empty contents (space or comment) used to
separate PDF objects.
-}
module Pdf.Parser.EmptyContent
  ( emptyContentP
  ) where

import           Control.Applicative            ( (<|>) )
import           Data.Binary.Parser             ( Get
                                                , label
                                                , satisfy
                                                , skipMany
                                                )
import           Pdf.Object.Object              ( isWhiteSpace )
import           Pdf.Parser.Comment             ( commentP )
import           Control.Monad                  ( void )

{-|
A binary parser meant to ignore empty content in a PDF file.
-}
emptyContentP :: Get ()
emptyContentP = label "emptycontent"
  $ skipMany (void (satisfy isWhiteSpace) <|> void commentP)
