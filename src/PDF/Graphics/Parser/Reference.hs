{- |
This module contains a parser for GFX references.

A GFX reference is an object value used to allow one object to refer to another.
It has the form “n m R” where n is an indirect object number, m is its
version number and R is the uppercase letter R.
-}
module PDF.Graphics.Parser.Reference
  ( referenceP
  ) where

import Data.Binary.Parser (Get, isDigit, label, satisfy, some', takeWhile1)
import Data.PDF.GFXObject (GFXObject (GFXReference), isKeywordCharacter)
import Data.Word (Word8)

import PDF.Graphics.Parser.EmptyContent (emptyContentP)

import Util.Number (toNumber)

digit :: Get Word8
digit = satisfy isDigit

integerP :: Get [Word8]
integerP = some' digit

{- |
Parse a `GFXReference`.
-}
referenceP :: Get GFXObject
referenceP = label "referenceG" $ do
  objectNumber <- toNumber <$> integerP
  emptyContentP
  revisionNumber <- toNumber <$> integerP
  emptyContentP
  keyword <- takeWhile1 isKeywordCharacter

  case keyword of
    "R" -> return $ GFXReference objectNumber revisionNumber
    _   -> fail "referenceP"
