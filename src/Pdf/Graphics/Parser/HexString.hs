{- |
This module contains a parser for PDF hexadecimal strings.

Hexadecimal strings are useful for including arbitrary binary data in a PDF
file.

A hexadecimal string shall be written as a sequence of hexadecimal digits
(0–9 and either A–F or a–f) encoded as ASCII characters and enclosed within
angle brackets (using LESS-THAN SIGN (3Ch) and GREATER-THAN SIGN (3Eh)):
`<4E6F762073686D6F7A206B6120706F702E>`

Each pair of hexadecimal digits defines one byte of the string. White-space
characters (such as SPACE (20h), HORIZONTAL TAB (09h), CARRIAGE RETURN (0Dh),
LINE FEED (0Ah), and FORM FEED (0Ch)) shall be ignored.

If the final digit of a hexadecimal string is missing—that is, if there is an
odd number of digits—the final digit shall be assumed to be 0.

`<901FA3>` is a 3-byte string consisting of the characters whose hexadecimal
codes are 90, 1F, and A3, but `<901FA>` is a 3-byte string containing the
characters whose hexadecimal codes are 90, 1F, and A0.
-}
module Pdf.Graphics.Parser.HexString
  ( hexStringP
  ) where

import Data.Binary.Parser (Get, isHexDigit, label, takeWhile1, word8)

import Pdf.Graphics.Object (GFXObject (GFXHexString), isWhiteSpace)

import Util.Ascii (asciiGREATERTHANSIGN, asciiLESSTHANSIGN)
import Util.String (normalizeHexString)

{- |
A binary parser for a GFX hexstring.

A GFX hexstring is a structure signaled by angle brackets.

It returns a `GFXHexString`.
-}
hexStringP :: Get GFXObject
hexStringP = label "hexstringG" $ do
  word8 asciiLESSTHANSIGN
  content <- takeWhile1 (\byte -> isHexDigit byte || isWhiteSpace byte)
  word8 asciiGREATERTHANSIGN
  return $ GFXHexString (normalizeHexString content)
