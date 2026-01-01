{-|
This module contains parser for empty contents (space or comment) used to
separate PDF objects.
-}
module PDF.Object.Parser.EmptyContent
  ( emptyContentP
  ) where

import Control.Applicative ((<|>))
import Control.Monad (void)

import Data.Binary.Parser (Get, label, satisfy, skipMany)

import PDF.Object.Object (isWhiteSpace)
import PDF.Object.Parser.Comment (commentP)

{-|
A binary parser meant to ignore empty content in a PDF file.

Empty content is either:

- white spaces (according to PDF specifications)
- comments (starting with a percent sign "%" and ending with a line return)
-}
emptyContentP :: Get ()
emptyContentP = label "emptycontent"
  $ skipMany (void (satisfy isWhiteSpace) <|> void commentP)
