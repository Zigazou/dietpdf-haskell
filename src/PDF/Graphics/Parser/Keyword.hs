{- |
This module contains a parser for GFX keywords.

A keyword is a sequence of alphabetical letters only (either lowercase or
uppercase).

This parser recognizes true, false and null keywords. Any other sequence is
considered generic keyword.
-}
module PDF.Graphics.Parser.Keyword
  ( keywordP
  ) where

import Data.Binary.Parser (Get, label)
import Data.Binary.Parser qualified as BP
import Data.Binary.Parser.Word8 (satisfy)
import Data.ByteString qualified as BS
import Data.PDF.GFXObject
  ( GFXObject (GFXBool, GFXNull, GFXOperator)
  , isKeywordCharacter
  , isKeywordFirstCharacter
  , toGSOperator
  )

{- |
A binary parser for a GFX keyword.

It returns a `GFXBool`, a `GFXNull` or a `GFXKeyword`.
-}
keywordP :: Get GFXObject
keywordP = label "keywordG" $ do
  firstChar <- satisfy isKeywordFirstCharacter
  nextChars <- BP.takeWhile isKeywordCharacter
  case BS.cons firstChar nextChars of
    "true"  -> return (GFXBool True)
    "false" -> return (GFXBool False)
    "null"  -> return GFXNull
    keyword -> return (GFXOperator (toGSOperator keyword))
