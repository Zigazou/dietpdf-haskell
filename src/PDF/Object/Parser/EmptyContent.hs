{-|
Parser for whitespace and comments in PDF documents

This module provides a binary parser for empty content in PDF documents. Empty
content includes whitespace and comments, which are used as separators between
PDF objects and are typically ignored during parsing.
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
Parse and discard empty content in a PDF document.

Empty content consists of any combination of:

- Whitespace characters (as defined by PDF specifications: space, tab, line
  feed, carriage return, form feed, and null)
- Comments (lines starting with '%' and ending with a line terminator)

This parser consumes zero or more occurrences of whitespace or comments and
always succeeds without consuming any significant data, returning unit @()@. It
is used to skip separators between PDF objects during parsing.
-}
emptyContentP :: Get ()
emptyContentP = label "emptycontent"
  $ skipMany (void (satisfy isWhiteSpace) <|> void commentP)
