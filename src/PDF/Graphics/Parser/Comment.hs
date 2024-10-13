{-|
This module contains a parser for Graphics comments.
-}
module PDF.Graphics.Parser.Comment
  ( commentP
  ) where

import Data.Binary.Parser (Get, label, takeTill, word8)
import Data.PDF.GFXObject (GFXObject (GFXComment))

import PDF.Graphics.Parser.LooseEndOfLine (isLooseEndOfLine, looseEndOfLineP)

import Util.Ascii (asciiPERCENTSIGN)

{-|
A binary parser for a graphics comment.

A graphics comment is a line starting with `asciiPERCENTSIGN`.

It returns a `GFXComment` for any other string
 -}
commentP :: Get GFXObject
commentP = label "commentG" $ do
  word8 asciiPERCENTSIGN
  comment <- takeTill isLooseEndOfLine
  looseEndOfLineP
  return $ GFXComment comment
