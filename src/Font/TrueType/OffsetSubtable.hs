{-|
TrueType/OpenType font offset subtable.

The offset subtable is the first structure in a TrueType/OpenType font file,
serving as the header that describes the font's table organization. It contains:

* The scaler type (identifying the font format: TrueType, CFF, etc.)
* The number of tables in the font
* Binary search optimization parameters for efficient table lookup

The search parameters (searchRange, entrySelector, rangeShift) enable binary
search through the table directory, which must be sorted by table tag. These
values are calculated based on the number of tables and must be kept in sync
when tables are added or removed.

The offset subtable is always 12 bytes in size:

* 4 bytes: scaler type
* 2 bytes: number of tables
* 2 bytes: search range
* 2 bytes: entry selector
* 2 bytes: range shift
-}
module Font.TrueType.OffsetSubtable
  ( OffsetSubtable
    ( OffsetSubtable
    , osScalerType
    , osNumTables
    , osSearchRange
    , osEntrySelector
    , osRangeShift
    )
  , fromOffsetSubtable
  ) where

import Data.Binary (Word16)
import Data.Binary.Put (putWord16be, runPut)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.HasWrittenSize (HasWrittenSize (writtenSize))
import Data.Kind (Type)

import Font.TrueType.ScalerType (ScalerType, fromScalerType)

{-|
Offset subtable header for a TrueType/OpenType font.

This is the first structure in a font file, located at the beginning. It
provides metadata about the font's structure and enables efficient binary search
through the table directory.

The three search optimization fields (osSearchRange, osEntrySelector,
osRangeShift) must be calculated based on osNumTables:

* Let maxPowerOf2 = largest power of 2 <= osNumTables
* osSearchRange = maxPowerOf2 * 16
* osEntrySelector = log2(maxPowerOf2)
* osRangeShift = osNumTables * 16 - osSearchRange

For example, with 12 tables (where maxPowerOf2 = 8):

* osSearchRange = 128 (8 * 16)
* osEntrySelector = 3 (log2(8))
* osRangeShift = 64 (12 * 16 - 128)
-}
type OffsetSubtable :: Type
data OffsetSubtable = OffsetSubtable
  { osScalerType    :: ScalerType -- ^ Scaler type identifying the font format (TrueType, CFF, etc.)
  , osNumTables     :: Word16     -- ^ Number of tables in the font
  , osSearchRange   :: Word16     -- ^ (Maximum power of 2 <= numTables) * 16
  , osEntrySelector :: Word16     -- ^ log2(maximum power of 2 <= numTables)
  , osRangeShift    :: Word16     -- ^ numTables * 16 - searchRange
  }
  deriving stock (Eq, Show)

{-|
  The written size of an offset subtable is always 12 bytes:

  * 4 bytes: scaler type (UInt32)
  * 2 bytes: numTables (UInt16)
  * 2 bytes: searchRange (UInt16)
  * 2 bytes: entrySelector (UInt16)
  * 2 bytes: rangeShift (UInt16)
-}
instance HasWrittenSize OffsetSubtable where
  writtenSize :: OffsetSubtable -> Int
  writtenSize _subtable = 12

{-|
Serialize an offset subtable to its binary representation.

Produces a 12-byte ByteString in big-endian format:

1. Scaler type (4 bytes) - identifies the font format
2. Number of tables (2 bytes)
3. Search range (2 bytes)
4. Entry selector (2 bytes)
5. Range shift (2 bytes)

The resulting ByteString can be written to the beginning of a font file,
followed by the table directory and table data.

Example:

> let subtable = OffsetSubtable truetype 12 128 3 64
> let bytes = fromOffsetSubtable subtable
> -- bytes is a 12-byte ByteString
-}
fromOffsetSubtable :: OffsetSubtable -> ByteString
fromOffsetSubtable (OffsetSubtable scaler numTables searchRange entrySelector rangeShift) =
  let scalerBytes = fromScalerType scaler
      metadataBytes = BSL.toStrict $ runPut $ do
        putWord16be numTables
        putWord16be searchRange
        putWord16be entrySelector
        putWord16be rangeShift
  in  scalerBytes <> metadataBytes
