{-|
TrueType font directory and table structures.

Defines the font directory structure containing offset subtables and table
entries, with utilities for loading table content and computing checksums
for validation.
-}
module Font.TrueType.FontDirectory
  ( TableDirectory
  , FontDirectory(FontDirectory, fdOffsetSubtable, fdTableDirectory)
  , OffsetSubtable
    ( OffsetSubtable
    , osScalerType
    , osNumTables
    , osSearchRange
    , osEntrySelector
    , osRangeShift
    )
  , TableEntry(TableEntry, teTag, teChecksum, teOffset, teLength, teData)
  , loadContent
  , calcChecksum
  , calcTableChecksum
  ) where

import Data.Array (Array)
import Data.Binary (Word16, Word32)
import Data.Binary.Parser (getWord32be, many', parseOnly)
import Data.Bits (shiftL)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Either (fromRight)
import Data.Kind (Type)

import Font.TrueType.FontTable
    ( FontTable (FTHead, FTRaw)
    , Head (hCheckSumAdjustment)
    , fromHead
    )
import Font.TrueType.Parser.Head (headP)
import Font.TrueType.ScalerType (ScalerType)
import Font.TrueType.TableIdentifier (TableIdentifier (RTTFontHeader))

{-|
Compute the checksum of raw binary data.

Sums all 32-bit big-endian words in the byte string, handling any trailing
bytes less than 4 bytes by left-padding with zeros. Returns the total sum.
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
Offset subtable header for a TrueType font.
-}
type OffsetSubtable :: Type
data OffsetSubtable = OffsetSubtable
  { osScalerType    :: ScalerType -- ^ Scaler to be used to rasterize this font
  , osNumTables     :: Word16 -- ^ Number of tables
  , osSearchRange   :: Word16 -- ^ Search range (max power of 2 <= numTables)*16
  , osEntrySelector :: Word16 -- ^ log2(maximum power of 2 <= numTables)
  , osRangeShift    :: Word16 -- ^ numTables*16-searchRange
  }
  deriving stock (Eq, Show)

{-|
Font table entry in the TrueType font directory.
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
Extract raw table data from a font file.

Extracts exactly @teLength@ bytes starting at @teOffset@ from the font
bytestring for the given table entry.
-}
getBytes :: ByteString -> TableEntry -> ByteString
getBytes bytes entry = BS.take
  (fromIntegral $ teLength entry)
  (BS.drop (fromIntegral $ teOffset entry) bytes)

{-|
Load and parse table content from font bytes.

Extracts raw bytes for a table entry and attempts to parse them according to
the table type. The head (font header) table is parsed into a structured format;
all other tables remain as raw 'FTRaw' data.
-}
loadContent :: ByteString -> TableEntry -> TableEntry
loadContent bytes entry@TableEntry { teTag = RTTFontHeader } =
  let raw = getBytes bytes entry
  in  case parseOnly headP raw of
        Left  _      -> entry { teData = FTRaw raw }
        Right teHead -> entry { teData = FTHead teHead }
loadContent bytes entry = entry { teData = FTRaw (getBytes bytes entry) }

{-|
Compute the checksum of a table entry.

Calculates the 32-bit checksum of the table data. For head tables, the
checksum is computed with 'hCheckSumAdjustment' set to 0 before calculation.
-}
calcTableChecksum :: TableEntry -> Word32
calcTableChecksum TableEntry { teData = FTRaw raw } = calcChecksum raw
calcTableChecksum TableEntry { teData = FTHead fontHead } =
  calcChecksum (fromHead fontHead { hCheckSumAdjustment = 0 })

type TableDirectory :: Type
type TableDirectory = Array TableEntry

type FontDirectory :: Type
data FontDirectory = FontDirectory
  { fdOffsetSubtable :: OffsetSubtable
  , fdTableDirectory :: TableDirectory
  }
  deriving stock (Eq, Show)
