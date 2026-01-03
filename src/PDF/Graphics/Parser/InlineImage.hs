{-|
Parser for PDF graphics stream inline images

This module provides a binary parser for inline images in PDF graphics streams.
Inline images are image data embedded directly within content streams using the
BI/ID/EI (Begin Image / Image Data / End Image) operators.
-}
module PDF.Graphics.Parser.InlineImage
  ( inlineImageP
  ) where

import Control.Applicative ((<|>))
import Control.Monad (when)

import Data.Binary.Parser (Get, label, manyTill, satisfy, sepBy, string, word8)
import Data.ByteString qualified as BS
import Data.Map.Strict (fromList)
import Data.PDF.GFXObject (GFXObject (GFXInlineImage), isWhiteSpace)

import PDF.Graphics.Parser.Container (dictionaryKeyValueP)
import PDF.Graphics.Parser.EmptyContent (emptyContentP)

import Util.Ascii (asciiCR, asciiLF)

{-|
Parse and discard a single whitespace character.

This parser recognizes a whitespace character as defined by 'isWhiteSpace'. If
the character is a carriage return (CR), it also consumes the following line
feed (LF) character if present, handling CR+LF line endings correctly.
-}
whiteSpaceP :: Get ()
whiteSpaceP = do
  byte <- satisfy isWhiteSpace
  when (byte == asciiCR) (word8 asciiLF)

{-|
Parse a PDF graphics stream inline image.

An inline image is a complete image object embedded within a content stream. It
consists of:

1. The @BI@ operator (Begin Image)
2. A dictionary containing image parameters (width, height, color space, etc.)
3. The @ID@ operator (Image Data) followed by whitespace
4. Raw image data bytes
5. The @EI@ operator (End Image) as a terminator

The image data is extracted as a byte sequence from the position after @ID@
until the @EI@ terminator is encountered. The @ID@ operator can optionally be
preceded by whitespace before the image data.

Returns a graphics inline image object containing the parsed image dictionary
and raw image data.
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
