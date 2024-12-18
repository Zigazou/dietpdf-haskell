{- |
This module provides a parser for PDF strings.

A string consists of a series of bytes (unsigned integer values in the range
0 to 255) and the bytes are not integer objects, but are stored in a more
compact form.

The text string type shall be used for character strings that contain
information intended to be human-readable, such as text annotations, bookmark
names, article names, document information, and so forth.

For text strings encoded in Unicode, the first two bytes shall be 254 followed
by 255. These two bytes represent the Unicode byte order marker, U+FEFF,
indicating that the string is encoded in the UTF-16BE (big-endian) encoding
scheme specified in the Unicode standard.
-}
module PDF.Object.Parser.String
  ( stringP
  ) where

import Control.Applicative ((<|>))

import Data.Binary.Parser (Get, label, many', satisfy, some', word8)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Maybe (catMaybes)
import Data.Word (Word8)

import PDF.Object.Object
    ( PDFObject (PDFString)
    , isOctal
    , isStringEscapeSequence
    , isStringRegularChar
    )
import PDF.Object.Parser.LooseEndOfLine (looseEndOfLineP)

import Util.Ascii
    ( asciiBS
    , asciiCR
    , asciiDIGITZERO
    , asciiFF
    , asciiHT
    , asciiLEFTPARENTHESIS
    , asciiLF
    , asciiREVERSESOLIDUS
    , asciiRIGHTPARENTHESIS
    , pattern AsciiLEFTPARENTHESIS
    , pattern AsciiLOWERB
    , pattern AsciiLOWERF
    , pattern AsciiLOWERN
    , pattern AsciiLOWERR
    , pattern AsciiLOWERT
    , pattern AsciiREVERSESOLIDUS
    , pattern AsciiRIGHTPARENTHESIS
    )

escapedEndOfLineP :: Get (Maybe Word8)
escapedEndOfLineP =
  word8 asciiREVERSESOLIDUS >> looseEndOfLineP >> return Nothing

escapedCharP :: Get (Maybe Word8)
escapedCharP = do
  word8 asciiREVERSESOLIDUS
  satisfy isStringEscapeSequence >>= convert
 where
  convert :: Word8 -> Get (Maybe Word8)
  convert AsciiLOWERN           = return (Just asciiLF)
  convert AsciiLOWERR           = return (Just asciiCR)
  convert AsciiLOWERT           = return (Just asciiHT)
  convert AsciiLOWERB           = return (Just asciiBS)
  convert AsciiLOWERF           = return (Just asciiFF)
  convert AsciiLEFTPARENTHESIS  = return (Just asciiLEFTPARENTHESIS)
  convert AsciiRIGHTPARENTHESIS = return (Just asciiRIGHTPARENTHESIS)
  convert AsciiREVERSESOLIDUS   = return (Just asciiREVERSESOLIDUS)
  convert _anyOtherCharacter    = fail "escapedChar"

escapedOctalP :: Get (Maybe Word8)
escapedOctalP =
  word8 asciiREVERSESOLIDUS >> threeOctal <|> twoOctal <|> oneOctal
 where
  digitToNumber x = x - asciiDIGITZERO
  threeOctal = do
    first  <- digitToNumber <$> satisfy isOctal
    second <- digitToNumber <$> satisfy isOctal
    third  <- digitToNumber <$> satisfy isOctal
    return $ Just (first * 64 + second * 8 + third)

  twoOctal = do
    first  <- digitToNumber <$> satisfy isOctal
    second <- digitToNumber <$> satisfy isOctal
    return $ Just (first * 8 + second)

  oneOctal = Just . digitToNumber <$> satisfy isOctal

dropEnd :: Int -> ByteString -> ByteString
dropEnd n ps | n <= 0            = ps
             | n >= BS.length ps = ""
             | otherwise         = BS.take (BS.length ps - n) ps

charP :: Get (Maybe Word8)
charP =
  escapedEndOfLineP
    <|> escapedCharP
    <|> escapedOctalP
    <|> (Just <$> satisfy isStringRegularChar)

charsP :: Get ByteString
charsP = BS.pack . catMaybes <$> some' charP

rawStringP :: Get ByteString
rawStringP = do
  word8 asciiLEFTPARENTHESIS
  content <- many' (rawStringP <|> charsP)
  word8 asciiRIGHTPARENTHESIS
  return $ BS.concat ["(", BS.concat content, ")"]

{- |
Parse a `PDFString`
-}
stringP :: Get PDFObject
stringP = label "string" (PDFString . BS.drop 1 . dropEnd 1 <$> rawStringP)
