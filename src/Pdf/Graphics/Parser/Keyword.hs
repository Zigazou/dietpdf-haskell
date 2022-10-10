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
                                                  , GFXKeyword
                                                  , GFXNull
                                                  )
                                                , isKeywordCharacter
                                                )

{- |
A binary parser for a GFX keyword.

It returns a `GFXBool`, a `GFXNull` or a `GFXKeyword`.
-}
keywordP :: Get GFXObject
keywordP = label "keyword" $ do
  keyword <- takeWhile1 isKeywordCharacter
  return $ case keyword of
    "true"  -> GFXBool True
    "false" -> GFXBool False
    "null"  -> GFXNull
    _       -> GFXKeyword keyword

{-
General graphics state w, J, j, M, d, ri, i, gs
Special graphics state q, Q, cm
Path construction m, l, c, v, y, h, re
Path painting S, s, f, F, f*, B, B*, b, b*, n
Clipping paths W, W*
Text objects BT, ET
Text state Tc, Tw, Tz, TL, Tf, Tr, Ts Text positioning Td, TD, Tm, T*
Text showing Tj, TJ, ', "
Type 3 fonts d0, d1
Color CS, cs, SC, SCN, sc, scn, G, g, RG, rg, K, k
Shading patterns sh
Inline images BI, ID, EI
XObjects Do
Marked content MP, DP, BMC, BDC, EMC
Compatibility BX, EX
-}