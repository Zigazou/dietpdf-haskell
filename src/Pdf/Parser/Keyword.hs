{-# LANGUAGE OverloadedStrings #-}
{- |
This module contains a parser for PDF keywords.

A keyword is a sequence of alphabetical letters only (either lowercase or
uppercase).

This parser recognizes true, false and null keywords. Any other sequence is
considered generic keyword.
-}
module Pdf.Parser.Keyword
  ( keywordP
  ) where

import           Data.Binary.Parser             ( Get
                                                , label
                                                , takeWhile1
                                                )
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFBool
                                                  , PDFKeyword
                                                  , PDFNull
                                                  )
                                                , isKeywordCharacter
                                                )

{- |
A binary parser for a PDF keyword.

It returns a `PDFBool`, a `PDFNull` or a `PDFKeyword`.
-}
keywordP :: Get PDFObject
keywordP = label "keyword" $ do
  keyword <- takeWhile1 isKeywordCharacter
  return $ case keyword of
    "true"  -> PDFBool True
    "false" -> PDFBool False
    "null"  -> PDFNull
    _       -> PDFKeyword keyword
