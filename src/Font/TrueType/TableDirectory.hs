{-|
TrueType font table directory structures and operations.

The table directory is a collection of table entries that make up a TrueType
font. This module provides operations for creating, manipulating, and
serializing table directories.

A table directory maintains an ordered sequence of table entries, where each
entry describes one table in the font (e.g., 'head', 'glyf', 'loca', etc.). The
entries are typically sorted by their tag identifiers when written to ensure
proper font structure.
-}
module Font.TrueType.TableDirectory
  ( TableDirectory
  , TableDirectory' (TableDirectory)
  , fromTableDirectory
  , fromTablesData
  , relativeOffsetsAndSizes
  , updateEntries
  , singleton
  , prepend
  , append
  , mkTableDirectory
  , filterTableDirectory
  , mapOnTableType
  ) where

import Data.Array (Array)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable.Extra (toList)
import Data.HasLength (HasLength (objectLength))
import Data.HasWrittenSize (HasWrittenSize (writtenSize))
import Data.Kind (Type)
import Data.Sequence (Seq (Empty, (:<|)), (<|), (|>))
import Data.Sequence qualified as Seq

import Font.TrueType.TableEntry
  (TableEntry (teTag), fromTableData, fromTableEntry, updateOffsetAndSize)
import Font.TrueType.TableIdentifier (TableIdentifier)

{-|
Polymorphic table directory type with phantom type parameter.

The phantom type parameter allows type-safe tracking of the entry type while
maintaining a single implementation. The role is nominal to prevent unwanted
type coercions.
-}
type role TableDirectory' nominal
type TableDirectory' :: Type -> Type
newtype TableDirectory' a = TableDirectory (Array a) deriving stock (Show, Eq)

{-|
Concrete table directory type for TrueType table entries.

This is the primary type used throughout the codebase for managing collections
of font tables.
-}
type TableDirectory :: Type
type TableDirectory = TableDirectory' TableEntry

{-|
Create a table directory from a list of table entries.

The entries are stored in the order provided, though they will be sorted by tag
when serialized.
-}
mkTableDirectory :: [TableEntry] -> TableDirectory
mkTableDirectory entries = TableDirectory (Seq.fromList entries)

{-|
Create a table directory containing a single entry.
-}
singleton :: TableEntry -> TableDirectory
singleton entry = TableDirectory (Seq.singleton entry)

{-|
Prepend an entry to the beginning of a table directory.

Note: The order here is logical; entries are sorted by tag when serialized.
-}
prepend :: TableEntry -> TableDirectory -> TableDirectory
prepend entry (TableDirectory tables) =
  TableDirectory (entry <| tables)

{-|
Append an entry to the end of a table directory.

Note: The order here is logical; entries are sorted by tag when serialized.
-}
append :: TableDirectory -> TableEntry -> TableDirectory
append (TableDirectory tables) entry =
  TableDirectory (tables |> entry)

{-|
Functor instance for TableDirectory'.

Allows mapping functions over the entries in a table directory.
-}
instance Functor TableDirectory' where
  fmap :: (a -> b) -> TableDirectory' a -> TableDirectory' b
  fmap f (TableDirectory tables) = TableDirectory (fmap f tables)

{-|
Monoid instance for TableDirectory'.

Empty directory and directory concatenation operations.
-}
instance Monoid (TableDirectory' a) where
  mempty :: TableDirectory' a
  mempty = TableDirectory mempty

{-|
Semigroup instance for TableDirectory'.

Allows combining two table directories by concatenating their entries.
-}
instance Semigroup (TableDirectory' a) where
  (<>) :: TableDirectory' a -> TableDirectory' a -> TableDirectory' a
  (TableDirectory t1) <> (TableDirectory t2) = TableDirectory (t1 <> t2)

{-|
Foldable instance for TableDirectory'.

Allows folding operations over the entries in a table directory.
-}
instance Foldable TableDirectory' where
  foldMap :: Monoid m => (a -> m) -> TableDirectory' a -> m
  foldMap f (TableDirectory tables) = foldMap f tables

{-|
HasWrittenSize instance for TableDirectory'.

Calculates the total size in bytes when the table directory entries are written
to the font file. This is the sum of all individual entry sizes.
-}
instance (HasWrittenSize a) => HasWrittenSize (TableDirectory' a) where
  writtenSize :: TableDirectory' a -> Int
  writtenSize (TableDirectory arr) =
    foldr (\entry acc -> acc + writtenSize entry) 0 arr

{-|
HasLength instance for TableDirectory'.

Returns the number of entries in the table directory.
-}
instance HasLength (TableDirectory' a) where
  objectLength :: TableDirectory' a -> Int
  objectLength (TableDirectory arr) = length arr

{-|
Filter a table directory to retain only entries matching a predicate.

This is useful for removing specific tables (e.g., hinting tables) or keeping
only required tables.

Example:

@ -- Remove all hinting-related tables filterTableDirectory (not .
isHintingTable . teTag) directory @
-}
filterTableDirectory :: (TableEntry -> Bool) -> TableDirectory -> TableDirectory
filterTableDirectory predicate (TableDirectory entries) =
  TableDirectory (Seq.filter predicate entries)

{-|
Apply a transformation function to a specific table type in the directory.

This function searches the table directory for entries matching the given table
identifier and applies the provided transformation function to them. All other
entries remain unchanged.

Parameters:
- Table directory to operate on
- Table identifier to match (e.g., 'glyf', 'head', 'loca')
- Transformation function to apply to matching entries

Returns:
- Updated table directory with the transformation applied

Example:

@
-- Update the checksum of the 'head' table
mapOnTableType directory (mkTableIdentifier "head") updateChecksum
@
-}
mapOnTableType
  :: TableDirectory
  -> TableIdentifier
  -> (TableEntry -> TableEntry)
  -> TableDirectory
mapOnTableType tableDirectory tableIdentifier fn = update <$> tableDirectory
  where
    update :: TableEntry -> TableEntry
    update tableEntry =
      if teTag tableEntry == tableIdentifier
        then fn tableEntry
        else tableEntry

{-|
Serialize the table directory entries to binary format.

Returns the raw binary representation of all table directory entries, which
appears immediately after the offset subtable in a TrueType font file. Each
entry contains the table tag, checksum, offset, and length.
-}
fromTableDirectory :: TableDirectory -> ByteString
fromTableDirectory (TableDirectory tablesEntry) =
  mconcat (toList (fromTableEntry <$> tablesEntry))

{-|
Align a size value to the next 4-byte boundary.

TrueType tables must be aligned to 4-byte boundaries. This function calculates
the aligned size by rounding up to the nearest multiple of 4.

Examples:

@
alignSizeTo4 0  == 0
alignSizeTo4 1  == 4
alignSizeTo4 4  == 4
alignSizeTo4 5  == 8
@
-}
alignSizeTo4 :: Int -> Int
alignSizeTo4 size = case size `mod` 4 of
  0         -> size
  remainder -> size + (4 - remainder)

{-|
Pad a ByteString to the next 4-byte boundary with null bytes.

TrueType requires all table data to be aligned to 4-byte boundaries. This
function adds 0-3 null bytes to ensure proper alignment.
-}
padTo4 :: ByteString -> ByteString
padTo4 bytes = case BS.length bytes `mod` 4 of
  0  -> bytes
  1  -> BS.append bytes "\x00\x00\x00"
  2  -> BS.append bytes "\x00\x00"
  _3 -> BS.append bytes "\x00"

{-|
Serialize all table data from the directory.

Returns the concatenated and 4-byte aligned data of all tables in the directory.
This is the actual table content that appears after the table directory in the
font file.

Each table's data is padded to a 4-byte boundary as required by the TrueType
specification.
-}
fromTablesData :: TableDirectory -> ByteString
fromTablesData (TableDirectory tablesEntry) =
  mconcat (toList (padTo4 . fromTableData <$> tablesEntry))

{-|
Calculate relative offsets and sizes for all tables in the directory.

Given a starting offset, this function computes the offset and data size for
each table entry. The offsets are relative to the start of the table data
section and account for 4-byte alignment between tables.

Parameters:
- Starting offset (typically the size of the offset subtable + table directory)
- Table directory

Returns:
- Array of (offset, size) pairs for each entry, in the same order as the
  directory

This is used when updating table entries with their correct offsets before
serializing the font.
-}
relativeOffsetsAndSizes :: Int -> TableDirectory -> Array (Int, Int)
relativeOffsetsAndSizes _anyStart (TableDirectory Empty) = mempty

relativeOffsetsAndSizes start (TableDirectory (entry :<| rest))
  = (start, dataSize)
  <| relativeOffsetsAndSizes (start + alignedSize) (TableDirectory rest)
 where
  dataSize = BS.length (fromTableData entry)
  alignedSize = alignSizeTo4 dataSize

{-|
Update all table entries with their correct offsets and sizes.

Sorts the entries by tag (as required by the TrueType specification), computes
the correct offset for each table based on the starting position and aligned
sizes, and updates each entry with this information.

The starting offset should be the position where table data begins in the font
file, which is after the offset subtable and table directory.

This function must be called before serializing a font to ensure all table
offsets are correct.
-}
updateEntries :: Int -> TableDirectory -> TableDirectory
updateEntries start (TableDirectory entries) = TableDirectory $
  Seq.zipWith updateOffsetAndSize
              (relativeOffsetsAndSizes start sortedDirectory)
              sortedEntries
 where
  sortedEntries = Seq.sortBy (\a b -> compare (teTag a) (teTag b)) entries
  sortedDirectory = TableDirectory sortedEntries
