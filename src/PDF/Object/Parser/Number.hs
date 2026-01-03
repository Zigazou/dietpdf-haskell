{-|
Parser for PDF numeric objects

This module provides a binary parser for numeric objects in PDF documents.

An integer shall be written as one or more decimal digits optionally preceded by
a sign.

The value shall be interpreted as a signed decimal integer and shall be
converted to an integer object:

- 123
- 43445
- +17
- -98
- 0

A real value shall be written as one or more decimal digits with an optional
sign and a leading, trailing, or embedded PERIOD (2Eh) (decimal point).

The value shall be interpreted as a real number and shall be converted to a real
object:

- 34.5
- -3.62
- +123.6
- 4.
- -.002
- 0.0

Wherever a real number is expected, an integer may be used instead.

For example, it is not necessary to write the number 1.0 in real format; the
integer 1 is sufficient.
-}
module PDF.Object.Parser.Number
  ( numberP
  ) where

import Control.Applicative ((<|>))

import Data.Binary.Parser (Get, isDigit, label, option, satisfy, some', word8)
import Data.List (foldl')
import Data.Word (Word8)

import PDF.Object.Object (PDFObject (PDFNumber), isPlusMinus)

import Util.Ascii (asciiDIGITZERO, asciiFULLSTOP, asciiHYPHENMINUS)

{-|
Parse a sign character (+/-) and return the corresponding transformation
function.

Returns the identity function if the sign is @+@, and the 'negate' function if
the sign is @-@. This allows the parsed sign to be applied to a numeric value.
-}
plusMinus :: Num a => Get (a -> a)
plusMinus = do
  sign <- satisfy isPlusMinus
  return $ if sign == asciiHYPHENMINUS then negate else id

{-|
Parse a decimal point (period) character.

Returns unit @()@ after consuming the period character. Used as a separator
between integer and decimal parts of a floating-point number.
-}
dot :: Get ()
dot = word8 asciiFULLSTOP

{-|
Parse a single decimal digit (0-9).

Returns the byte value of the digit character.
-}
digit :: Get Word8
digit = satisfy isDigit

{-|
Convert a list of digit bytes into the integer part of a number.

Accumulates the digits from left to right, multiplying the accumulated value by
10 for each new digit. For example, digits [1, 2, 3] with initial value 0
results in ((0 * 10 + 1) * 10 + 2) * 10 + 3 = 123.0.

__Parameters:__

- The list of digit bytes to process
- The initial accumulator value (typically 0.0)

__Returns:__ The accumulated numeric value as a 'Double'.
-}
integerPart :: [Word8] -> Double -> Double
integerPart ys x = foldl'
  (\number decDigit -> number * 10.0 + fromIntegral (decDigit - asciiDIGITZERO))
  x
  ys

{-|
Convert a list of digit bytes into the fractional part of a number.

Accumulates the digits from left to right, dividing each new digit by an
increasing power of 10. For example, digits [5] with divisor 10 results in 0.5;
digits [1, 2, 3] result in 0.123.

__Parameters:__

- The list of digit bytes to process
- The accumulated digit value (typically 0.0 initially)
- The divisor (power of 10 for fractional position)

__Returns:__ The final fractional value as a 'Double'.
-}
decimalPart :: [Word8] -> Double -> Double -> Double
decimalPart [] x divisor = x / divisor
decimalPart (y : ys) x divisor =
  decimalPart ys (x * 10.0 + fromIntegral (y - asciiDIGITZERO)) (divisor * 10.0)

{-|
Combine integer and decimal digit parts into a single numeric value.

The integer part is converted to its numeric value, and the decimal part is
converted to its fractional value, then the two are summed.

__Parameters:__

- List of integer part digits (may be empty for numbers like @.5@)
- List of decimal part digits (may be empty for numbers like @4.@)

__Returns:__ The complete numeric value as a 'Double'.
-}
toNumber :: [Word8] -> [Word8] -> Double
toNumber leftPart rightPart =
  integerPart leftPart 0.0 + decimalPart rightPart 0.0 1.0

{-|
Parse a decimal number starting with a period (e.g., @.5@, @.002@).

This parser handles numbers that have no integer part, only a decimal point
followed by digits. The integer part is treated as empty.

__Returns:__ A tuple of (empty integer part, decimal digits).
-}
decimalP :: Get ([Word8], [Word8])
decimalP = dot >> some' digit >>= \rightPart -> return ([], rightPart)

{-|
Parse an integer with an optional decimal part.

This parser handles numbers with an integer part (required) and optionally a
period and decimal digits. Examples:

- @123@ returns ([1,2,3], [])
- @4.5@ returns ([4], [5])
- @4.@ returns ([4], [])
- @4@ returns ([4], [])

__Returns:__ A tuple of (integer digits, decimal digits). The decimal part list
is empty if there is no period or no digits after the period.
-}
integerDecimalP :: Get ([Word8], [Word8])
integerDecimalP = do
  leftPart  <- some' digit
  rightPart <- (dot >> some' digit) <|> (dot >> return []) <|> return []
  return (leftPart, rightPart)

{-|
Parse a `PDFNumber`.

Internally, all numbers (either integer or real) are stored as `Double`.

>>> parseOnly numberP "123"
Right (PDFNumber 123.0)

>>> parseOnly numberP "43445"
Right (PDFNumber 43445.0)

>>> parseOnly numberP "+17"
Right (PDFNumber 17.0)

>>> parseOnly numberP "-98"
Right (PDFNumber (-98.0))

>>> parseOnly numberP "0"
Right (PDFNumber 0.0)

>>> parseOnly numberP "34.5"
Right (PDFNumber 34.5)

>>> parseOnly numberP "-3.62"
Right (PDFNumber (-3.62))

>>> parseOnly numberP "+123.6"
Right (PDFNumber 123.6)

>>> parseOnly numberP "4."
Right (PDFNumber 4.0)

>>> parseOnly numberP "-.002"
Right (PDFNumber (-0.002))

>>> parseOnly numberP "0.0"
Right (PDFNumber 0.0)
-}
numberP :: Get PDFObject
numberP = label "number" $ do
  sign                  <- option id plusMinus
  (leftPart, rightPart) <- integerDecimalP <|> decimalP
  let number = toNumber leftPart rightPart
  return $ PDFNumber (sign number)
