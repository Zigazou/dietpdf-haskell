-- | This module contains parsers for GFX inline images.
module Pdf.Graphics.Parser.InlineImage
  ( inlineImageP
  ) where

import Control.Applicative ((<|>))
import Control.Monad (when)

import Data.Binary.Parser (Get, label, manyTill, satisfy, sepBy, string, word8)
import Data.ByteString qualified as BS
import Data.Map.Strict (fromList)
import Data.PDF.GFXObject (GFXObject (GFXInlineImage), isWhiteSpace)

import Pdf.Graphics.Parser.Container (dictionaryKeyValueP)
import Pdf.Graphics.Parser.EmptyContent (emptyContentP)

import Util.Ascii (asciiCR, asciiLF)

whiteSpaceP :: Get ()
whiteSpaceP = do
  byte <- satisfy isWhiteSpace
  when (byte == asciiCR) (word8 asciiLF)

{-|
A binary parser for a GFX inline image.
-}
inlineImageP :: Get GFXObject
inlineImageP = label "inlineimageG" $ do
  string "BI"
  emptyContentP

  dictionary <- fromList <$> sepBy dictionaryKeyValueP emptyContentP
  emptyContentP

  string "ID"
  whiteSpaceP

  image <- manyTill (satisfy (const True))
                    (string "EI" <|> whiteSpaceP *> string "EI")

  return $ GFXInlineImage dictionary (BS.pack image)
