{-|
Parser for whitespace and comments in PDF graphics streams

This module provides a binary parser for empty content in PDF graphics streams.
Empty content includes whitespace and comments, which are used as separators
between PDF objects and are typically ignored during parsing.
-}
module PDF.Graphics.Parser.EmptyContent
  ( emptyContentP
  ) where

import Control.Applicative ((<|>))
import Control.Monad (void)

import Data.Binary.Parser (Get, label, satisfy, skipMany)

import PDF.Graphics.Parser.Comment (commentP)
import PDF.Object.Object (isWhiteSpace)

{-|
Parse and discard empty content in PDF graphics streams.

Empty content consists of any combination of:

- whitespace characters (as defined by PDF specifications)
- comments (lines starting with '%' and ending with a line terminator)

This parser is used to skip separators between PDF objects and consumes zero or
more occurrences of whitespace or comments. It always succeeds without consuming
any significant data, returning unit @()@.
-}
emptyContentP :: Get ()
emptyContentP = label "emptycontentG"
  $ skipMany (void (satisfy isWhiteSpace) <|> void commentP)
