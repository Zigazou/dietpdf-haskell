{- |
This module contains a parser for PDF names.

A name object is an atomic symbol uniquely defined by a sequence of any
characters (8-bit values) except null (character code 0).

A SOLIDUS (2Fh) (/) is used to introduce a name. The SOLIDUS is not part of the
name but is a prefix indicating that what follows is a sequence of characters
representing the name in the PDF file.

A NUMBER SIGN (23h) (#) in a name shall be written by using its 2-digit
hexadecimal code (23), preceded by the NUMBER SIGN.

Any character in a name that is a regular character (other than NUMBER SIGN)
shall be written as itself or by using its 2-digit hexadecimal code, preceded by
the NUMBER SIGN.

Any character that is not a regular character shall be written using its 2-digit
hexadecimal code, preceded by the NUMBER SIGN only.

White space used as part of a name shall always be coded using the 2-digit
hexadecimal notation and no white space may intervene between the SOLIDUS and
the encoded name.

Regular characters that are outside the range EXCLAMATION MARK(21h) (!) to
TILDE (7Eh) (~) should be written using the hexadecimal notation.
-}
module Pdf.Object.Parser.Name
  ( nameP
  ) where

import           Control.Applicative            ( (<|>) )
import           Data.Binary.Parser             ( Get
                                                , isHexDigit
                                                , label
                                                , satisfy
                                                , some'
                                                , word8
                                                )
import qualified Data.ByteString               as BS
import           Data.Word                      ( Word8 )
import           Pdf.Object.Object              ( PDFObject(PDFName)
                                                , isNameRegularChar
                                                )
import           Util.Ascii                     ( asciiDIGITNINE
                                                , asciiDIGITZERO
                                                , asciiLOWERA
                                                , asciiLOWERF
                                                , asciiNUMBERSIGN
                                                , asciiSOLIDUS
                                                , asciiUPPERA
                                                )

hexadecimalCodeP :: Get Word8
hexadecimalCodeP = do
  word8 asciiNUMBERSIGN
  first  <- hexDigitToNumber <$> satisfy isHexDigit
  second <- hexDigitToNumber <$> satisfy isHexDigit
  let code = first * 16 + second
  if code == 0 then fail "hexadecimalCode" else return code
 where
  hexDigitToNumber x
    | x >= asciiDIGITZERO && x <= asciiDIGITNINE = x - asciiDIGITZERO
    | x >= asciiLOWERA && x <= asciiLOWERF = 10 + x - asciiLOWERA
    | otherwise                            = 10 + x - asciiUPPERA

charP :: Get Word8
charP = hexadecimalCodeP <|> satisfy isNameRegularChar

{- |
Parse a `PDFName`.

The name resulting from the parsing is decoded (there is no hexadecimal value
in the resulting bytestring).

>>> parseOnly nameP "/Name1"
Right (PDFName "Name1")

>>> parseOnly nameP "/ASomewhatLongerName"
Right (PDFName "ASomewhatLongerName")

>>> parseOnly nameP "/A;Name_With-Various***Characters?"
Right (PDFName "A;Name_With-Various***Characters?")

>>> parseOnly nameP "/1.2"
Right (PDFName "1.2")

>>> parseOnly nameP "/$$"
Right (PDFName "$$")

>>> parseOnly nameP "/@pattern"
Right (PDFName "@pattern")

>>> parseOnly nameP "/.notdef"
Right (PDFName ".notdef")

>>> parseOnly nameP "/lime#20Green"
Right (PDFName "Lime Green")

>>> parseOnly nameP "/paired#28#29parentheses"
Right (PDFName "paired()parentheses")

>>> parseOnly nameP "/The_Key_of_F#23_Minor"
Right (PDFName "The_Key_of_F#_Minor")

>>> parseOnly nameP "/A#42"
Right (PDFName "AB")
-}
nameP :: Get PDFObject
nameP = label "name" $ do
  word8 asciiSOLIDUS
  name <- BS.pack <$> some' charP
  return $ PDFName name
