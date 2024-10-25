{-|
This module contains a parser for loose end of line (either CR, CR+LF or LF).
-}
module PDF.Object.Parser.LooseEndOfLine
  ( looseEndOfLineP
  , isLooseEndOfLine
  ) where

import Control.Applicative ((<|>))

import Data.Binary.Parser (Get, word8)
import Data.Word (Word8)

import Util.Ascii
    ( asciiCR
    , asciiLF
    , asciiSPACE
    , pattern AsciiCR
    , pattern AsciiLF
    )

-- | Returns True if a loose end of line is found, False otherwise
isLooseEndOfLine :: Word8 -> Bool
isLooseEndOfLine AsciiCR            = True
isLooseEndOfLine AsciiLF            = True
isLooseEndOfLine _anyOtherCharacter = False

-- | Parser for a loose end of line (CR, CR+LF or LF)
looseEndOfLineP :: Get ()
looseEndOfLineP =
  (word8 asciiCR >> word8 asciiLF)
    <|> (word8 asciiSPACE >> word8 asciiCR)
    <|> (word8 asciiSPACE >> word8 asciiLF)
    <|> word8 asciiCR
    <|> word8 asciiLF
