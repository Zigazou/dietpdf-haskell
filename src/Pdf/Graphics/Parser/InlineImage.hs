{-# LANGUAGE OverloadedStrings #-}
-- | This module contains parsers for GFX inline images.
module Pdf.Graphics.Parser.InlineImage
  ( inlineImageP
  ) where

import           Control.Applicative            ( (<|>) )
import qualified Data.ByteString               as BS
import           Data.Binary.Parser             ( Get
                                                , label
                                                , sepBy
                                                , word8
                                                , string
                                                , satisfy
                                                , manyTill
                                                )
import           Data.Map.Strict                ( fromList )
import           Pdf.Graphics.Object            ( GFXObject(GFXInlineImage)
                                                , isWhiteSpace
                                                )
import           Pdf.Graphics.Parser.Container  ( dictionaryKeyValueP )
import           Pdf.Graphics.Parser.EmptyContent
                                                ( emptyContentP )
import           Util.Ascii                     ( asciiCR
                                                , asciiLF
                                                )
import           Control.Monad                  ( when )

whiteSpaceP :: Get ()
whiteSpaceP = do
  byte <- satisfy isWhiteSpace
  when (byte == asciiCR) (word8 asciiLF)

{-|
A binary parser for a GFX inline image.
-}
inlineImageP :: Get GFXObject
inlineImageP = label "inline-image" $ do
  string "BI"
  emptyContentP

  dictionary <- fromList <$> sepBy dictionaryKeyValueP emptyContentP
  emptyContentP

  string "ID"
  whiteSpaceP

  image <- manyTill (satisfy (const True))
                    (string "EI" <|> whiteSpaceP *> string "EI")

  return $ GFXInlineImage dictionary (BS.pack image)
