{-|
TrueType font table entry structures and operations.

A table entry is a record in the font's table directory that describes a single
table in the font file. Each entry contains metadata about the table (tag,
checksum, offset, length) and optionally the parsed or raw table data.

The table directory appears near the beginning of a TrueType font file,
immediately after the offset subtable. Each entry is exactly 16 bytes and
contains:

- Tag: 4-byte ASCII identifier (e.g., \"head\", \"glyf\", \"loca\")
- Checksum: 32-bit checksum of the table data for validation
- Offset: Byte offset from the start of the font file to this table
- Length: Length of the table in bytes

This module provides functions for creating, manipulating, checksumming, and
serializing table entries.
-}
module Font.TrueType.TableEntry
  ( TableEntry(TableEntry, teTag, teChecksum, teOffset, teLength, teData)
  , loadContent
  , calcChecksum
  , updateTableEntry
  , fromTableEntry
  , getBytes
  , fromTableData
  , updateOffsetAndSize
  ) where

import Data.Binary (Word32)
import Data.Binary.Parser (getWord32be, many', parseOnly)
import Data.Binary.Put (putByteString, putWord32be, runPut)
import Data.Bits (shiftL)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Either (fromRight)
import Data.HasWrittenSize (HasWrittenSize (writtenSize))
import Data.Kind (Type)

import Font.TrueType.FontTable
  (FontTable (FTGlyf, FTHead, FTLoca, FTRaw), fromFontTable)
import Font.TrueType.FontTable.HeadTable
  (HeadTable (hCheckSumAdjustment), fromHeadTable)
import Font.TrueType.Parser.Head (headP)
import Font.TrueType.TableIdentifier
  (TableIdentifier (RTTFontHeader, RTTGlyphData), fromTableIdentifier)

{-|
Compute the checksum of raw binary data.

Sums all 32-bit big-endian words in the byte string, handling any trailing bytes
less than 4 bytes by left-padding with zeros. Returns the total sum.

The checksum algorithm:
1. Treat the data as a sequence of 32-bit big-endian unsigned integers
2. Sum all complete 32-bit words
3. For any remaining 1-3 bytes, left-pad with zeros to form a final 32-bit word
4. Return the sum (overflow wraps around)

This is the standard TrueType checksum algorithm used to validate table
integrity.
-}
calcChecksum :: ByteString -> Word32
calcChecksum raw =
  foldr (+) lastValue . fromRight [] . parseOnly (many' getWord32be) $ raw
 where
  lastValue :: Word32
  lastValue =
    case BS.unpack (BS.drop (BS.length raw - (BS.length raw `rem` 4)) raw) of
      []     -> 0
      [a]    -> fromIntegral a `shiftL` 24
      [a, b] -> (fromIntegral a `shiftL` 24) + (fromIntegral b `shiftL` 16)
      [a, b, c] ->
        (fromIntegral a `shiftL` 24)
          + (fromIntegral b `shiftL` 16)
          + (fromIntegral c `shiftL` 8)
      _anyError -> 0

{-|
Font table entry in the TrueType font directory.

Each table entry is a 16-byte record that describes one table in the font. The
entries are stored in the table directory, typically sorted by tag in ascending
order.

Fields:

- 'teTag': 4-byte table identifier (e.g., 'head', 'glyf', 'loca')
- 'teChecksum': 32-bit checksum for validating table integrity
- 'teOffset': Byte offset from start of font file to the table data
- 'teLength': Length of the table data in bytes (unpadded)
- 'teData': The actual table content, either parsed or raw

The 'teData' field can contain either structured parsed data (for tables like
'head') or raw unparsed bytes ('FTRaw').
-}
type TableEntry :: Type
data TableEntry = TableEntry
  { teTag      :: TableIdentifier -- ^ 4-byte identifier
  , teChecksum :: Word32 -- ^ Checksum for this table
  , teOffset   :: Word32 -- ^ Offset from beginning of sfnt
  , teLength   :: Word32 -- ^ Length of this table in byte (actual length)
  , teData     :: FontTable -- ^ Raw table data
  }
  deriving stock (Eq, Show)

{-|
Ord instance for TableEntry.

Entries are ordered by their tag identifier, which is the standard ordering
used in TrueType font files.
-}
instance Ord TableEntry where
  compare :: TableEntry -> TableEntry -> Ordering
  compare entry1 entry2 = compare (teTag entry1) (teTag entry2)

{-|
HasWrittenSize instance for TableEntry.

Each table entry is always exactly 16 bytes when written to a font file,
regardless of the table's actual data size. The 16 bytes consist of:

- 4 bytes: table tag
- 4 bytes: checksum
- 4 bytes: offset
- 4 bytes: length
-}
instance HasWrittenSize TableEntry where
  writtenSize :: TableEntry -> Int
  writtenSize _entry = 16 -- Each table entry is always 16 bytes

{-|
Update a table entry's checksum based on its data.

Recalculates the checksum for the table's data and updates the entry. Special
handling for different table types:

- 'FTRaw': Checksum of raw data
- 'FTHead': Checksum with checkSumAdjustment field set to 0 (per spec)
- 'FTLoca': Checksum of serialized location table
- 'FTGlyf': Checksum of serialized glyph table

This should be called whenever table data is modified to maintain integrity.
-}
updateTableEntry :: TableEntry -> TableEntry
updateTableEntry entry@(TableEntry { teData = FTRaw raw }) =
  entry { teChecksum = calcChecksum raw }
updateTableEntry entry@(TableEntry { teData = FTHead fontHead }) =
  entry { teChecksum = calcChecksum (fromHeadTable fontHead { hCheckSumAdjustment = 0 }) }
updateTableEntry entry@(TableEntry { teData = FTLoca locTable }) =
  entry { teChecksum = calcChecksum (fromFontTable (FTLoca locTable)) }
updateTableEntry entry@(TableEntry { teData = FTGlyf glyfTable }) =
  entry { teChecksum = calcChecksum (fromFontTable (FTGlyf glyfTable)) }

{-|
Update a table entry's offset and length.

Sets the offset and length fields based on calculated values from the table
directory layout. This is used when building or reconstructing a font file to
ensure all entries point to the correct locations.

Parameters:
- (offset, byteSize): The calculated offset and size for this table
- entry: The table entry to update

The offset is relative to the start of the font file, and the size is the actual
unpadded length of the table data.
-}
updateOffsetAndSize :: (Int, Int) -> TableEntry -> TableEntry
updateOffsetAndSize (offset, byteSize) entry =
  entry { teOffset = fromIntegral offset, teLength = fromIntegral byteSize }

{-|
Serialize a table entry to its 16-byte binary representation.

Converts the table entry metadata (tag, checksum, offset, length) to the binary
format used in the table directory. The table data itself is not included; only
the directory entry.

Returns exactly 16 bytes in the format:

@ [4 bytes: tag] [4 bytes: checksum] [4 bytes: offset] [4 bytes: length] @

All multi-byte values are in big-endian format as required by TrueType.
-}
fromTableEntry :: TableEntry -> ByteString
fromTableEntry (TableEntry tag checksum tableOffset tableLength _data) =
  BSL.toStrict $ runPut $ do
    putByteString (fromTableIdentifier tag)
    putWord32be checksum
    putWord32be tableOffset
    putWord32be tableLength

{-|
Extract the table data as a ByteString.

Serializes the table data to its binary representation, handling different table
types appropriately:

- 'FTRaw': Returns the raw bytes as-is
- 'FTHead': Serializes the head table structure
- 'FTLoca': Serializes the location table
- 'FTGlyf': Serializes the glyph table

This returns the actual table data without padding. Use 'padTo4' from
TableDirectory if 4-byte alignment is needed.
-}
fromTableData :: TableEntry -> ByteString
fromTableData (TableEntry { teData = FTRaw raw })       = raw
fromTableData (TableEntry { teData = FTHead fontHead }) = fromHeadTable fontHead
fromTableData (TableEntry { teData = FTLoca locTable }) = fromFontTable (FTLoca locTable)
fromTableData (TableEntry { teData = FTGlyf glyfTable }) = fromFontTable (FTGlyf glyfTable)

{-|
Extract raw table data from a font file.

Extracts exactly @teLength@ bytes starting at @teOffset@ from the font
bytestring for the given table entry.

Parameters:
- bytes: The complete font file as a ByteString
- entry: The table entry containing offset and length information

Returns the raw table data. This function does not parse or validate the data;
it simply extracts the bytes specified by the entry.
-}
getBytes :: ByteString -> TableEntry -> ByteString
getBytes bytes entry = BS.take
  (fromIntegral $ teLength entry)
  (BS.drop (fromIntegral $ teOffset entry) bytes)

{-|
Load and parse table content from font bytes.

Extracts raw bytes for a table entry and attempts to parse them according to the
table type. Currently supported parsed formats:

- 'head' table: Parsed into structured 'HeadTable' with all metadata
- All other tables: Kept as raw 'FTRaw' data

Special notes:

- The 'glyf' table cannot be parsed without the 'loca' table (which provides
  offsets for individual glyphs), so it remains as raw data
- Failed parsing falls back to raw data to ensure the font can still be
  processed

This function is typically called during font loading to extract and parse table
data from the complete font file.

Parameters:
- bytes: The complete font file as a ByteString
- entry: The table entry with offset/length but unloaded data

Returns the entry with 'teData' populated with either parsed or raw data.
-}
loadContent :: ByteString -> TableEntry -> TableEntry
loadContent bytes entry@TableEntry { teTag = RTTFontHeader } =
  let raw = getBytes bytes entry
  in  case parseOnly headP raw of
        Left  _      -> entry { teData = FTRaw raw }
        Right teHead -> entry { teData = FTHead teHead }

loadContent bytes entry@TableEntry { teTag = RTTGlyphData } =
  -- The glyf table cannot be parsed without the loca table
  -- Keep it as raw data for now
  entry { teData = FTRaw (getBytes bytes entry) }

loadContent bytes entry = entry { teData = FTRaw (getBytes bytes entry) }
