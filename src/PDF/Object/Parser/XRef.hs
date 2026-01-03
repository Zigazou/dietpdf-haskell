{-|
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
module PDF.Object.Parser.XRef
  ( xrefP
  ) where

import Data.Binary.Parser
  ( Get
  , anyWord8
  , isDigit
  , label
  , satisfy
  , scan
  , skipWhile
  , some'
  , string
  , word8
  )
import Data.ByteString qualified as BS
import Data.Ix (inRange)
import Data.Word (Word8)

import PDF.Object.Object
  ( PDFObject (PDFXRef)
  , XRefEntry (XRefEntry)
  , XRefState (FreeEntry, InUseEntry)
  , XRefSubsection (XRefSubsection)
  , isWhiteSpace
  )
import PDF.Object.Parser.EmptyContent (emptyContentP)
import PDF.Object.Parser.LooseEndOfLine (looseEndOfLineP)

import Util.Ascii
  (asciiDIGITNINE, asciiDIGITZERO, asciiLOWERF, asciiLOWERN, asciiSPACE)
import Util.Number (toNumber)

{-|
Parse a variable-width integer.

Parses one or more consecutive decimal digits and converts them to an integer
value. The width is determined by the number of consecutive digit characters
found in the input.

__Returns:__ The integer value of the parsed digits.
-}
integerP :: Get Int
integerP = toNumber <$> some' (satisfy isDigit)

{-|
Parse a fixed-width integer with exactly @width@ digits.

Attempts to scan exactly @width@ bytes, each of which must be a decimal digit.
Fails if fewer than @width@ digits are found.

This is used for parsing fixed-format xref entries where fields like byte
offsets (10 digits) and generation numbers (5 digits) have strict width
requirements.

__Parameters:__

- The required number of digit characters

__Returns:__ The integer value represented by the fixed-width digit sequence.

__Fails:__ If fewer than @width@ consecutive digits are found.
-}
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

{-|
Apply a parser @count@ times and collect results in a list.

If @count@ is 0, returns an empty list immediately. Otherwise, applies the
parser once, collects the result, then recursively applies the parser @count -
1@ more times.

This is useful for parsing a known number of fixed-format entries, such as the
xref entries in a subsection where the entry count is specified in the
subsection header.

__Parameters:__

- The number of times to apply the parser
- The parser to apply repeatedly

__Returns:__ A list of @count@ parsed values.
-}
takeN :: Int -> Get a -> Get [a]
takeN 0     _      = return []
takeN count parser = do
  item  <- parser
  items <- takeN (count - 1) parser
  return (item : items)

{-|
Parse the state indicator of an xref entry.

Reads a single byte and determines whether it represents a free or in-use entry:

- @f@ (lowercase): Free entry (object has been deleted)
- @n@ (lowercase): In-use entry (object is current)

__Returns:__ The 'XRefState' indicating whether the entry is free or in-use.

__Fails:__ If the byte is neither 'f' nor 'n'.
-}
xrefStateP :: Get XRefState
xrefStateP = anyWord8 >>= validate
 where
  validate :: Word8 -> Get XRefState
  validate value | value == asciiLOWERF = return FreeEntry
                 | value == asciiLOWERN = return InUseEntry
                 | otherwise            = fail "xrefstate"

{-|
Parse a single cross-reference entry.

A cross-reference entry is exactly 20 bytes and contains:

1. 10-digit byte offset (or next free object number for free entries)
2. Space character
3. 5-digit generation number
4. Space character
5. State indicator: 'f' (free) or 'n' (in-use)
6. End-of-line marker (whitespace)

Examples:

- @0000000000 65535 f eol@ - Free entry, next free is object 0
- @0000025325 00000 n eol@ - In-use entry at byte offset 25325

__Returns:__ An 'XRefEntry' containing the offset (or next free), generation
number, and state.
-}
xrefEntryP :: Get XRefEntry
xrefEntryP = do
  offset <- fixedSizeInteger 10
  word8 asciiSPACE
  generation <- fixedSizeInteger 5
  word8 asciiSPACE
  state <- xrefStateP
  skipWhile isWhiteSpace
  return $ XRefEntry offset generation state

{-|
Parse a cross-reference subsection.

A subsection describes a contiguous range of objects and consists of:

1. Optional leading whitespace
2. Starting object number
3. Space and count (number of entries in this subsection)
4. Empty content (whitespace/comments)
5. Exactly @count@ xref entries

For example:

@
0 6
0000000000 65535 f
0000000018 00000 n
...
@

The starting object number and count allow multiple subsections with non-
contiguous object ranges (common in incrementally updated files).

__Returns:__ An 'XRefSubsection' containing the starting object number, entry
count, and the parsed entries.
-}
xrefSubsectionP :: Get XRefSubsection
xrefSubsectionP = do
  skipWhile isWhiteSpace
  offset <- integerP
  word8 asciiSPACE
  count <- integerP
  emptyContentP
  entries <- takeN count xrefEntryP
  return $ XRefSubsection offset count entries

{-|
Parse a complete PDF cross-reference table.

A cross-reference table begins with the keyword "xref" followed by one or more
subsections. Each subsection describes a contiguous range of indirect objects
and their locations within the file.

The parser:

1. Matches the keyword "xref"
2. Consumes line terminators
3. Parses one or more subsections (the first usually starting at object 0)

For non-incrementally-updated files, there is typically one subsection covering
all objects. For incrementally-updated files, multiple xref sections may exist
in the file, with the last one providing access to the current object versions.

__Returns:__ A PDF xref object containing the list of parsed subsections.

__Fails:__ If the keyword "xref" is not found or a subsection format is invalid.
-}
xrefP :: Get PDFObject
xrefP = label "xref" $ do
  string "xref"
  looseEndOfLineP
  PDFXRef <$> some' xrefSubsectionP
