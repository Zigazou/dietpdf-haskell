{-# LANGUAGE OverloadedStrings #-}

{- |
This module provides a parser for PDF cross-reference tables.

The cross-reference table contains information that permits random access to
indirect objects within the file so that the entire file need not be read to
locate any particular object. The table shall contain a one-line entry for
each indirect object, specifying the byte offset of that object within the body
of the file.

Each cross-reference section shall begin with a line containing the keyword
xref.

Following this line shall be one or more cross-reference subsections, which
may appear in any order.

For a file that has never been incrementally updated, the cross-reference
section shall contain only one subsection, whose object numbering begins at 0.

Each cross-reference subsection shall contain entries for a contiguous range of
object numbers.

The subsection shall begin with a line containing two numbers separated by a
SPACE (20h), denoting the object number of the first object in this subsection
and the number of entries in the subsection.

Following this line are the cross-reference entries themselves, one per line.

Each entry shall be exactly 20 bytes long, including the end-of-line marker.

There are two kinds of cross-reference entries: one for objects that are in use
and another for objects that have been deleted and therefore are free.
Both types of entries have similar basic formats, distinguished by the
keyword n (for an in-use entry) or f (for a free entry).

The format of an in-use entry shall be "nnnnnnnnnn ggggg n eol" where:

- nnnnnnnnnn shall be a 10-digit byte offset in the decoded stream
- ggggg shall be a 5-digit generation number
- n shall be a keyword identifying this as an in-use entry
- eol shall be a 2-character end-of-line sequence

The cross-reference entry for a free object has essentially the same format,
except that the keyword shall be f instead of n and the interpretation of the
first item is different "nnnnnnnnnn ggggg f eol" where:

- nnnnnnnnnn shall be the 10-digit object number of the next free object
- ggggg shall be a 5-digit generation number
- f shall be a keyword identifying this as a free entry
- eol shall be a 2-character end-of-line sequence
-}
module Pdf.Object.Parser.XRef
  ( xrefP
  ) where

import           Data.Binary.Parser             ( Get
                                                , anyWord8
                                                , isDigit
                                                , label
                                                , satisfy
                                                , scan
                                                , some'
                                                , string
                                                , word8
                                                , skipWhile
                                                )
import           Pdf.Object.Parser.LooseEndOfLine
                                                ( looseEndOfLineP )
import           Data.Word                      ( Word8 )
import qualified Data.ByteString               as BS
import           Data.Ix                        ( inRange )
import           Pdf.Object.Object              ( PDFObject(PDFXRef)
                                                , XRefEntry(XRefEntry)
                                                , XRefState
                                                  ( FreeEntry
                                                  , InUseEntry
                                                  )
                                                , XRefSubsection(XRefSubsection)
                                                , isSpace
                                                )
import           Util.Ascii                     ( asciiCR
                                                , asciiDIGITNINE
                                                , asciiDIGITZERO
                                                , asciiLF
                                                , asciiLOWERF
                                                , asciiLOWERN
                                                , asciiSPACE
                                                )
import           Util.Number                    ( toNumber )
import           Pdf.Object.Parser.EmptyContent ( emptyContentP )

integerP :: Get Int
integerP = toNumber <$> some' (satisfy isDigit)

fixedSizeInteger :: Int -> Get Int
fixedSizeInteger width = do
  digits <- scan width getNDigit
  if BS.length digits == width
    then return . toNumber . BS.unpack $ digits
    else fail "fixedSizeInteger"
 where
  getNDigit :: Int -> Word8 -> Maybe Int
  getNDigit 0 _ = Nothing
  getNDigit n byte
    | inRange (asciiDIGITZERO, asciiDIGITNINE) byte = Just (n - 1)
    | otherwise = Nothing

takeN :: Int -> Get a -> Get [a]
takeN 0     _      = return []
takeN count parser = do
  item  <- parser
  items <- takeN (count - 1) parser
  return (item : items)

xrefStateP :: Get XRefState
xrefStateP = anyWord8 >>= validate
 where
  validate :: Word8 -> Get XRefState
  validate value | value == asciiLOWERF = return FreeEntry
                 | value == asciiLOWERN = return InUseEntry
                 | otherwise            = fail "xrefstate"

xrefEndOfLine :: Get ()
xrefEndOfLine = do
  byte1 <- anyWord8
  byte2 <- anyWord8
  validate (byte1, byte2)
 where
  validate :: (Word8, Word8) -> Get ()
  validate value | value == (asciiSPACE, asciiCR) = return ()
                 | value == (asciiSPACE, asciiLF) = return ()
                 | value == (asciiCR, asciiLF)    = return ()
                 | otherwise                      = fail "xrefendofline"

xrefEntryP :: Get XRefEntry
xrefEntryP = do
  offset <- fixedSizeInteger 10
  word8 asciiSPACE
  generation <- fixedSizeInteger 5
  word8 asciiSPACE
  state <- xrefStateP
  xrefEndOfLine
  return $ XRefEntry offset generation state

xrefSubsectionP :: Get XRefSubsection
xrefSubsectionP = do
  skipWhile isSpace
  offset <- integerP
  word8 asciiSPACE
  count <- integerP
  emptyContentP
  entries <- takeN count xrefEntryP
  return $ XRefSubsection offset count entries

{- |
Parse a `PDFXRef` object.
-}
xrefP :: Get PDFObject
xrefP = label "xref" $ do
  string "xref"
  looseEndOfLineP
  PDFXRef <$> some' xrefSubsectionP
