{-# LANGUAGE OverloadedStrings #-}
{- |
This module contains a parser for GFX keywords.

A keyword is a sequence of alphabetical letters only (either lowercase or
uppercase).

This parser recognizes true, false and null keywords. Any other sequence is
considered generic keyword.
-}
module Pdf.Graphics.Parser.Keyword
  ( keywordP
  ) where

import           Data.Binary.Parser             ( Get
                                                , label
                                                , takeWhile1
                                                )
import           Pdf.Graphics.Object            ( GFXObject
                                                  ( GFXBool
                                                  , GFXNull
                                                  , GFXOperator
                                                  )
                                                , isKeywordCharacter
                                                , toGSOperator
                                                )

{- |
A binary parser for a GFX keyword.

It returns a `GFXBool`, a `GFXNull` or a `GFXKeyword`.
-}
keywordP :: Get GFXObject
keywordP = label "keywordG" $ do
  keyword <- takeWhile1 isKeywordCharacter
  return $ case keyword of
    "true"  -> GFXBool True
    "false" -> GFXBool False
    "null"  -> GFXNull
    value   -> GFXOperator (toGSOperator value)
