{- |
This module provides a parser for GFX numbers.

An integer shall be written as one or more decimal digits optionally preceded
by a sign.

The value shall be interpreted as a signed decimal integer and shall be
converted to an integer object:

- 123
- 43445
- +17
- -98
- 0

A real value shall be written as one or more decimal digits with an optional
sign and a leading, trailing, or embedded PERIOD (2Eh) (decimal point).

The value shall be interpreted as a real number and shall be converted to a
real object:

- 34.5
- -3.62
- +123.6
- 4.
- -.002
- 0.0

Wherever a real number is expected, an integer may be used instead.

For example, it is not necessary to write the number 1.0 in real format;
the integer 1 is sufficient.
-}
module Pdf.Graphics.Parser.Number
  ( numberP
  ) where

import Control.Applicative ((<|>))

import Data.Binary.Parser
    ( Get
    , isDigit
    , label
    , option
    , satisfy
    , some'
    , word8
    )
import Data.List (foldl')
import Data.Word (Word8)

import Pdf.Graphics.Object (GFXObject (GFXNumber), isPlusMinus)

import Util.Ascii (asciiDIGITZERO, asciiFULLSTOP, asciiHYPHENMINUS)

plusMinus :: Num a => Get (a -> a)
plusMinus = do
  sign <- satisfy isPlusMinus
  return $ if sign == asciiHYPHENMINUS then negate else id

dot :: Get ()
dot = word8 asciiFULLSTOP

digit :: Get Word8
digit = satisfy isDigit

integerPart :: [Word8] -> Double -> Double
integerPart ys x = foldl'
  (\number decDigit -> number * 10.0 + fromIntegral (decDigit - asciiDIGITZERO))
  x
  ys

decimalPart :: [Word8] -> Double -> Double -> Double
decimalPart [] x divisor = x / divisor
decimalPart (y : ys) x divisor =
  decimalPart ys (x * 10.0 + fromIntegral (y - asciiDIGITZERO)) (divisor * 10.0)

toNumber :: [Word8] -> [Word8] -> Double
toNumber leftPart rightPart =
  integerPart leftPart 0.0 + decimalPart rightPart 0.0 1.0

decimalP :: Get ([Word8], [Word8])
decimalP = dot >> some' digit >>= \rightPart -> return ([], rightPart)

integerDecimalP :: Get ([Word8], [Word8])
integerDecimalP = do
  leftPart  <- some' digit
  rightPart <- (dot >> some' digit) <|> (dot >> return []) <|> return []
  return (leftPart, rightPart)

{- |
Parse a `GFXNumber`.

Internally, all numbers (either integer or real) are stored as `Double`.

>>> parseOnly numberP "123"
Right (GFXNumber 123.0)

>>> parseOnly numberP "43445"
Right (GFXNumber 43445.0)

>>> parseOnly numberP "+17"
Right (GFXNumber 17.0)

>>> parseOnly numberP "-98"
Right (GFXNumber (-98.0))

>>> parseOnly numberP "0"
Right (GFXNumber 0.0)

>>> parseOnly numberP "34.5"
Right (GFXNumber 34.5)

>>> parseOnly numberP "-3.62"
Right (GFXNumber (-3.62))

>>> parseOnly numberP "+123.6"
Right (GFXNumber 123.6)

>>> parseOnly numberP "4."
Right (GFXNumber 4.0)

>>> parseOnly numberP "-.002"
Right (GFXNumber (-0.002))

>>> parseOnly numberP "0.0"
Right (GFXNumber 0.0)
-}
numberP :: Get GFXObject
numberP = label "numberG" $ do
  sign                  <- option id plusMinus
  (leftPart, rightPart) <- integerDecimalP <|> decimalP
  let number = toNumber leftPart rightPart
  return $ GFXNumber (sign number)
