{-|
Conversion of cross-reference tables to bytestring representation

This module provides functions to convert PDF cross-reference (xref) structures
into their serialized bytestring form. The xref section is a critical part of
PDF files that enables fast random access to objects within the file.
-}
module PDF.Object.Object.FromXRef
  ( fromXRef
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.PDF.XRefEntry (XRefEntry (xreGeneration, xreOffset, xreState))
import Data.PDF.XRefState (XRefState (InUseEntry))
import Data.PDF.XRefSubsection
    ( XRefSubsection (xrssCount, xrssEntries, xrssStart)
    )
import Data.Text qualified as TS
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy qualified as TL

import Formatting (format, int, left, (%))
import Formatting.ByteStringFormatter (utf8)

{-|
Convert a single cross-reference entry to its bytestring representation.

An xref entry is a 20-byte line in the xref table containing:

1. A 10-digit byte offset (padded with leading zeros)
2. A space separator
3. A 5-digit generation number (padded with leading zeros)
4. A space separator
5. A single character indicating the entry state: 'n' for in-use or 'f' for free
6. A carriage return and line feed (CRLF) line terminator

Example: @0000000000 65535 f\r\n@ or @0000025325 00000 n\r\n@

Returns the formatted bytestring representation of a single xref entry.
-}
fromXRefEntry :: XRefEntry -> ByteString
fromXRefEntry xre = encodeUtf8 . TL.toStrict $ format
  (left 10 '0' % " " % left 5 '0' % " " % utf8 % "\r\n")
  (xreOffset xre)
  (xreGeneration xre)
  (if xreState xre == InUseEntry then "n" else "f")

{-|
Convert a cross-reference subsection to its bytestring representation.

An xref subsection begins with a header line containing:

1. The starting object number
2. A space
3. The count of entries in this subsection

Followed by all xref entries for the objects in the subsection, formatted
according to 'fromXRefEntry'.

Multiple subsections can exist in a single xref table, allowing discontinuous
ranges of object numbers.

Returns the formatted bytestring representation of a complete subsection.
-}
fromXRefSubsection :: XRefSubsection -> ByteString
fromXRefSubsection xrss = BS.concat
  [encodeUtf8 subsectionInfo, BS.concat (fromXRefEntry <$> xrssEntries xrss)]
 where
  subsectionInfo :: TS.Text
  subsectionInfo = TL.toStrict
    $ format ("" % int % " " % int % "\n") (xrssStart xrss) (xrssCount xrss)

{-|
Convert a complete cross-reference table to its bytestring representation.

An xref table consists of:

1. The keyword @xref@ on its own line
2. One or more subsections, each starting with a header line containing the
   starting object number and entry count
3. Individual xref entries for each object

The xref table enables PDF readers to quickly locate objects in a PDF file
without parsing the entire document sequentially.

__Parameters:__

- A list of xref subsections to serialize

__Returns:__ The complete xref table as a bytestring, starting with the @xref@
keyword and including all subsections and their entries.
-}
fromXRef :: [XRefSubsection] -> ByteString
fromXRef xrss = BS.concat ["xref\n", BS.concat (fmap fromXRefSubsection xrss)]
