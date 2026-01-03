{-|
Parser for PDF graphics stream comments

This module provides a binary parser for PDF graphics stream comments. Graphics
comments are text lines that start with a percent sign (%) and extend to the end
of the line. Comments are ignored during PDF processing but are preserved in the
data structure.
-}
module PDF.Graphics.Parser.Comment
  ( commentP
  ) where

import Data.Binary.Parser (Get, label, takeTill, word8)
import Data.PDF.GFXObject (GFXObject (GFXComment))

import PDF.Graphics.Parser.LooseEndOfLine (isLooseEndOfLine, looseEndOfLineP)

import Util.Ascii (asciiPERCENTSIGN)

{-|
Parse a graphics stream comment.

A graphics comment is a line that starts with the percent sign character ('%').
The comment extends from the percent sign until the end of the line (inclusive
of any platform-specific line terminator).

The parser returns a 'GFXComment' object containing the comment content (all
bytes between the percent sign and the line terminator).
-}
commentP :: Get GFXObject
commentP = label "commentG" $ do
  word8 asciiPERCENTSIGN
  comment <- takeTill isLooseEndOfLine
  looseEndOfLineP
  return $ GFXComment comment
