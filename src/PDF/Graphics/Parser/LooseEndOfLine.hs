{-|
This module contains a parser for loose end of line (either CR, CR+LF or LF).
-}
module PDF.Graphics.Parser.LooseEndOfLine
  ( looseEndOfLineP
  , isLooseEndOfLine
  ) where

import Control.Applicative ((<|>))

import Data.Binary.Parser (Get, word8)
import Data.Word (Word8)

import Util.Ascii (asciiCR, asciiLF)

-- | Returns True if a loose end of line is found, False otherwise
isLooseEndOfLine :: Word8 -> Bool
isLooseEndOfLine value | value == asciiCR = True
                       | value == asciiLF = True
                       | otherwise        = False

-- | Parser for a loose end of line (CR, CR+LF or LF)
looseEndOfLineP :: Get ()
looseEndOfLineP =
  (word8 asciiCR >> word8 asciiLF) <|> word8 asciiCR <|> word8 asciiLF
