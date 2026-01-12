{-|
TrueType font directory and table structures.

This module provides the core representation of a TrueType/OpenType font's
directory structure, which serves as the font file's table of contents.
The font directory consists of:

* An offset subtable containing metadata about the number of tables and
  search optimization parameters
* A table directory listing all tables in the font with their tags, checksums,
  offsets, and lengths

The module provides utilities for:

* Loading and serializing font directories
* Updating directory metadata when tables are added or removed
* Filtering and removing unnecessary tables to optimize font size
* Computing search parameters for efficient table lookup

The 'updateStructure' function automatically recalculates all metadata
when the table directory changes, ensuring consistency between the offset
subtable parameters and the actual table count and layout.
-}
module Font.TrueType.FontDirectory
  ( FontDirectory(FontDirectory, fdOffsetSubtable, fdTableDirectory)
  , updateStructure
  , fromFontDirectory
  , deleteTables
  , optimizeFontDirectory
  ) where

import Data.ByteString (ByteString)
import Data.HasLength (HasLength (objectLength))
import Data.HasWrittenSize (HasWrittenSize (writtenSize))
import Data.Kind (Type)
import Data.Set (Set)
import Data.Set qualified as Set

import Font.TrueType.OffsetSubtable
  ( OffsetSubtable (osEntrySelector, osNumTables, osRangeShift, osSearchRange)
  , fromOffsetSubtable
  )
import Font.TrueType.TableDirectory
  ( TableDirectory
  , filterTableDirectory
  , fromTableDirectory
  , fromTablesData
  , updateEntries
  )
import Font.TrueType.TableEntry (teTag)
import Font.TrueType.TableIdentifier
  ( TableIdentifier (OOTBaseline, OOTColor, OOTColorBitmapData, OOTColorBitmapLocation, OOTColorPalette, OOTDigitalSignature, OOTDigitalSignature, OOTEmbeddedBitmapData, OOTEmbeddedBitmapLocation, OOTGlyphDefinition, OOTGlyphPositioning, OOTGlyphSubstitution, OOTJustification, OOTLinearThreshold, OOTSVG, OOTVerticalDeviceMetrics, OTTAnchorPointTable, OTTBaseline, OTTBitmapData, OTTBitmapLocation, OTTControlValueProgram, OTTCrossReference, OTTEmbeddedBitmapScaling, OTTExtendedGlyphMetamorphosis, OTTExtendedKerning, OTTExtendedKerning, OTTFeatureName, OTTFontProgram, OTTGlyphMetamorphosis, OTTGlyphsInformation, OTTGridFittingAndScanConversion, OTTHorizontalDeviceMetrics, OTTKerning, OTTKerning, OTTLanguageTags, OTTLigatureCaret, OTTMetadata, OTTOpticalBounds, OTTStandardBitmapGraphics, OTTTracking)
  )

{-|
The complete font directory structure for a TrueType/OpenType font.

A font directory consists of two main components:

* The offset subtable: contains metadata about table count and search
  optimization parameters (searchRange, entrySelector, rangeShift)
* The table directory: a list of table entries, each describing one table in the
  font (name tag, checksum, offset, length)

The offset subtable parameters must be kept in sync with the actual number of
tables. Use 'updateStructure' after modifying the table directory to ensure
consistency.
-}
type FontDirectory :: Type
data FontDirectory = FontDirectory
  { fdOffsetSubtable :: OffsetSubtable
    -- ^ The offset subtable containing table count and search parameters
  , fdTableDirectory :: TableDirectory
    -- ^ The directory of table entries describing each table in the font
  }
  deriving stock (Eq, Show)

instance HasWrittenSize FontDirectory where
  writtenSize :: FontDirectory -> Int
  writtenSize (FontDirectory subtable directory) =
    writtenSize subtable + writtenSize directory

{-|
Find the largest power of 2 that is less than or equal to the given number.

Used to compute the searchRange and entrySelector fields in the offset subtable.
For example:

>>> maxPowerOf2 12 8 maxPowerOf2 16 16 maxPowerOf2 0 0
 -}
maxPowerOf2 :: Int -> Int
maxPowerOf2 n
  | n < 1     = 0
  | otherwise = go 1
  where
    go p
      | p * 2 > n = p
      | otherwise = go (p * 2)

{-|
Update the font directory structure to ensure consistency.

This function recalculates all metadata in the offset subtable based on the
current table directory contents:

* Updates 'osNumTables' to match the actual number of tables
* Computes 'osSearchRange' as (maxPowerOf2 numTables) * 16
* Computes 'osEntrySelector' as log2(maxPowerOf2 numTables)
* Computes 'osRangeShift' as numTables * 16 - searchRange
* Updates table offsets to account for the offset subtable and table directory
  sizes

Call this function after adding or removing tables from the directory to ensure
the search optimization parameters are correct and table offsets are properly
updated.

The search parameters enable binary search of the table directory, which is
required to be sorted by tag for efficient lookup.
-}
updateStructure :: FontDirectory -> FontDirectory
updateStructure fontDir@(FontDirectory subtable directory) =
  let
    tablesOffset :: Int
    tablesOffset = writtenSize fontDir

    tablesCount :: Int
    tablesCount = objectLength directory

    -- Calculate the maximum power of 2 less than or equal to tablesCount.
    limit :: Int
    limit = maxPowerOf2 tablesCount

    -- (max power of 2 <= numTables)*16
    searchRange :: Int
    searchRange = 16 * limit

    -- log2(maximum power of 2 <= numTables)
    entrySelector :: Int
    entrySelector = floor (logBase 2.0 (fromIntegral limit :: Double))

    -- numTables*16-searchRange
    rangeShift :: Int
    rangeShift = tablesCount * 16 - (limit * 16)
  in
    FontDirectory
      { fdOffsetSubtable = subtable
          { osNumTables     = fromIntegral tablesCount
          , osSearchRange   = fromIntegral searchRange
          , osEntrySelector = fromIntegral entrySelector
          , osRangeShift    = fromIntegral rangeShift
          }
      , fdTableDirectory = updateEntries tablesOffset directory
      }

{-|
Serialize a font directory to its binary representation.

Produces a complete font file by:

1. Updating the directory structure to ensure consistency
2. Serializing the offset subtable
3. Serializing the table directory (list of table entries)
4. Appending the actual table data

The resulting ByteString represents a valid TrueType/OpenType font that can be
written to a file.

Note: This function calls 'updateStructure' internally, so you don't need to
call it separately before serialization.
-}
fromFontDirectory :: FontDirectory -> ByteString
fromFontDirectory fontDirectory =
  let FontDirectory subtable directory = updateStructure fontDirectory
  in fromOffsetSubtable subtable
      <> fromTableDirectory directory
      <> fromTablesData directory

{-|
Remove specified tables from a font directory.

This function filters out any tables whose identifiers are in the provided set.
The offset subtable is preserved but should be updated via 'updateStructure'
after table removal to ensure metadata consistency.

Example:

> let tagsToRemove = Set.fromList [OTTKerning, OTTFontProgram] let slimmedFont =
> deleteTables tagsToRemove fontDir

Note: This function does not automatically call 'updateStructure'. You may want
to use 'optimizeFontDirectory' instead, which removes a predefined set of tables
and updates the structure.
-}
deleteTables :: Set TableIdentifier -> FontDirectory -> FontDirectory
deleteTables tagsToDelete (FontDirectory subtable directory) =
  let
    filteredDirectory :: TableDirectory
    filteredDirectory = filterTableDirectory
      (\entry -> not (teTag entry `Set.member` tagsToDelete))
      directory
  in
    FontDirectory subtable filteredDirectory

{-|
Set of table identifiers that can be safely removed for font optimization.

This set includes tables that are typically unnecessary for PDF embedding:

* Hinting tables: hdmx, LTSH, VDMX, fpgm, prep, gasp, cvt
* Layout tables: GDEF, GPOS, GSUB, BASE, JSTF (OpenType), and morx, mort, feat,
  trak, opbd, bsln, lcar, kerx, ankr (Apple AAT)
* Bitmap/color tables: CBDT, CBLC, EBDT, EBLC, bloc, bdat, sbix, SVG, COLR,
  CPAL, EBSC
* Metadata tables: meta, ltag, DSIG, xref, Gloc

These tables are used for advanced typography, hinting, color glyphs, and
metadata, which are often not needed when embedding fonts in PDFs where the
primary concern is glyph outlines for rendering.
-}
removableTables :: Set TableIdentifier
removableTables = Set.fromList
  [ OTTHorizontalDeviceMetrics
  , OOTLinearThreshold
  , OOTVerticalDeviceMetrics
  , OTTFontProgram
  , OTTGridFittingAndScanConversion
  , OOTDigitalSignature
  , OTTKerning
  , OTTControlValueProgram

    -- Layout OpenType
  , OOTGlyphDefinition
  , OOTGlyphPositioning
  , OOTGlyphSubstitution
  , OOTBaseline
  , OOTJustification

    -- Layout Apple/AAT
  , OTTGlyphMetamorphosis
  , OTTExtendedGlyphMetamorphosis
  , OTTFeatureName
  , OTTTracking
  , OTTOpticalBounds
  , OTTBaseline
  , OTTLigatureCaret

    -- Kerning & tables associées au layout
  , OTTKerning
  , OTTExtendedKerning
  , OTTAnchorPointTable

    -- Bitmap, colors, SVG
  , OOTColorBitmapData
  , OOTColorBitmapLocation
  , OOTEmbeddedBitmapData
  , OOTEmbeddedBitmapLocation
  , OTTBitmapData
  , OTTBitmapLocation
  , OTTStandardBitmapGraphics
  , OOTSVG
  , OOTColor
  , OOTColorPalette
  , OTTEmbeddedBitmapScaling

  -- Miscellaneous and meta tables
  , OTTMetadata
  , OTTLanguageTags
  , OOTDigitalSignature
  , OTTCrossReference
  , OTTGlyphsInformation
  ]

{-|
Optimize a font directory by removing unnecessary tables.

This function removes all tables listed in 'removableTables', which includes
hinting, layout, bitmap, color, and metadata tables that are typically
unnecessary for PDF embedding.

The optimization is useful for:

* Reducing font file size in PDFs
* Removing advanced typography features not needed for simple text rendering
* Stripping metadata and digital signatures

After removing tables, the font will still contain the essential tables needed
for rendering (such as glyf, loca, head, hhea, hmtx, maxp, name, post, and
cmap).

Example:

> let optimizedFont = optimizeFontDirectory originalFont let fontBytes =
> fromFontDirectory optimizedFont
-}
optimizeFontDirectory :: FontDirectory -> FontDirectory
optimizeFontDirectory = deleteTables removableTables
