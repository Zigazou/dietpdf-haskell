{-|
TrueType location table (loca) representation and serialization.

The location table provides an index into the glyph data (glyf table) by storing
the byte offset of each glyph. This enables efficient random access to
individual glyphs without parsing the entire glyph table.

== Structure

The location table contains n+1 offsets for n glyphs:

* Offsets [0..n-1]: Starting byte offset of each glyph
* Offset [n]: End of last glyph (total size of glyf table)

The length of a glyph can be determined by subtracting consecutive offsets:

> glyphLength i = offset[i+1] - offset[i]

A zero-length glyph (offset[i] == offset[i+1]) indicates an empty glyph with no
outline data.

== Format

There are two possible formats for the location table:

* Short format: 16-bit offsets divided by 2 (for fonts with glyf < 128KB)
* Long format: 32-bit offsets (for larger fonts)

This module implements the long format only. The format is specified in the font
header's 'indexToLocFormat' field (0 = short, 1 = long).
-}
module Font.TrueType.FontTable.LocationTable
    ( LocationTable
        ( LocationTable
        , ltOffsets
        )
    , fromLocationTable
    , glyphSlices
    , bestFormat
    ) where

import Data.Binary.Put (putWord16be, putWord32be, runPut)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Kind (Type)
import Data.Word (Word32)

import Font.TrueType.LocationTableFormat
  (LocationTableFormat (LongFormat, ShortFormat))

{-|
TrueType location table representation.

Stores byte offsets for locating each glyph within the glyf table data. The
table must have exactly numGlyphs + 1 entries, where numGlyphs is specified in
the maxp (maximum profile) table.

Offsets must be in ascending order (though consecutive offsets can be equal for
empty glyphs). All offsets should be multiples of 4 for optimal performance,
though this is not strictly required by the specification.

== Example

For a font with 3 glyphs where:

* Glyph 0: 20 bytes
* Glyph 1: 0 bytes (empty)
* Glyph 2: 35 bytes

The offsets would be: [0, 20, 20, 55]
-}
type LocationTable :: Type
newtype LocationTable = LocationTable
  { ltOffsets :: [Word32] -- ^ Offsets to glyph data
  }
  deriving stock (Eq, Show)

glyphSlices :: LocationTable -> Int -> [(Word32, Word32)]
glyphSlices (LocationTable (first : second : remain)) dataSize =
  zip (first : second : remain) (second : remain ++ [fromIntegral dataSize])
glyphSlices (LocationTable [first]) dataSize = [(first, fromIntegral dataSize)]
glyphSlices (LocationTable []) _anyDataSize  = []

bestFormat :: LocationTable -> LocationTableFormat
bestFormat (LocationTable offsets) =
  if all (\offset -> even offset && offset <= 0xFFFF * 2) offsets
    then ShortFormat
    else LongFormat

{-|
Serialize a LocationTable to binary ByteString.

Converts the offset array to the binary format specified by the TrueType font
specification using the long format (32-bit offsets). Each offset is written as
a big-endian Word32.

The resulting ByteString has length: (number of offsets) * 4 bytes.

This corresponds to indexToLocFormat = 1 in the head table. For fonts with small
glyph tables (< 128KB), the short format (indexToLocFormat = 0) would be more
space-efficient, but is not currently implemented.

== Output Format

Each offset is serialized as 4 bytes in big-endian order:

> [offset0_byte0, offset0_byte1, offset0_byte2, offset0_byte3, offset1_byte0,
>  offset1_byte1, ...]
-}
fromLocationTable :: LocationTable -> ByteString
fromLocationTable locTable = case bestFormat locTable of
  ShortFormat -> BSL.toStrict
    $ runPut
    $ mapM_ putWord16be ((`div` 2) . fromIntegral <$> ltOffsets locTable)

  LongFormat -> BSL.toStrict
    $ runPut
    $ mapM_ putWord32be (ltOffsets locTable)
