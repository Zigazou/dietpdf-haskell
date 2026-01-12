{-|
TrueType and OpenType font table identifiers.

This module provides a comprehensive enumeration of all standard table types
defined in the TrueType and OpenType specifications. Each table in a font file
is identified by a unique 4-byte ASCII tag (e.g., 'head', 'glyf', 'cmap').

The module includes:

* The 'TableIdentifier' type representing all known table types
* Conversion functions between tags and identifiers
* Custom 'Ord' instance that orders tables by their logical dependencies

== Naming Convention

Constructor names use prefixes to indicate the table's origin and status:

* @RTT@: Required TrueType tables (e.g., 'RTTGlyphData' for "glyf")
* @OTT@: Optional TrueType/Apple AAT tables (e.g., 'OTTKerning' for "kern")
* @OOT@: Optional OpenType tables (e.g., 'OOTGlyphPositioning' for "GPOS")

Required tables are essential for a valid font; optional tables provide
additional features like advanced typography, color glyphs, or hinting.

== Table Ordering

The 'Ord' instance orders tables by their logical dependencies and processing
order, not alphabetically. Core tables (glyf, loca, head) come first, followed
by metrics, layout, and finally metadata tables.

Source:
- https://developer.apple.com/fonts/TrueType-Reference-Manual/
- https://learn.microsoft.com/en-us/typography/opentype/spec/
-}
module Font.TrueType.TableIdentifier
  ( TableIdentifier(..)
  , fromTableIdentifier
  , toTableIdentifier
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Kind (Type)

{-|
TrueType and OpenType font table identifiers.

Enumerates all standard table types defined by Apple TrueType and Microsoft
OpenType specifications. Each constructor represents a 4-byte tag identifying a
specific font table. Tables marked as required are essential for valid fonts;
optional tables enhance functionality or support advanced features.

The 'OUnknownIdentifier' constructor wraps unrecognized 4-byte tags, allowing
the system to handle proprietary or future table types gracefully.

See module documentation for specification references and naming conventions.
-}
type TableIdentifier :: Type
data TableIdentifier
  = OTTAccentAttachment               -- ^ 'acnt' (optional)
  | OTTAnchorPointTable               -- ^ 'ankr' (optional)
  | OTTAxisVariation                  -- ^ 'avar' (optional)
  | OOTBaseline                       -- ^ 'BASE' (optional)
  | OTTBitmapData                     -- ^ 'bdat' (optional)
  | OTTBitmapFontHeader               -- ^ 'bhed' (optional)
  | OTTBitmapLocation                 -- ^ 'bloc' (optional)
  | OTTBaseline                       -- ^ 'bsln' (optional)
  | OOTColorBitmapData                -- ^ 'CBDT' (optional)
  | OOTColorBitmapLocation            -- ^ 'CBLC' (optional)
  | OOTCompactFontFormat              -- ^ 'cff ' (optional)
  | RTTCharacterToGlyphMappin         -- ^ 'cmap' (required)
  | OOTColor                          -- ^ 'COLR' (optional)
  | OOTColorPalette                   -- ^ 'CPAL' (optional)
  | OTTCVTVariations                  -- ^ 'cvar' (optional)
  | OTTControlValue                   -- ^ 'cvt ' (optional)
  | OOTDigitalSignature               -- ^ 'DSIG' (optional)
  | OOTEmbeddedBitmapData             -- ^ 'EBDT' (optional)
  | OOTEmbeddedBitmapLocation         -- ^ 'EBLC' (optional)
  | OTTEmbeddedBitmapScaling          -- ^ 'EBSC' (optional)
  | OTTFontDescriptors                -- ^ 'fdsc' (optional)
  | OTTFeatureName                    -- ^ 'feat' (optional)
  | OTTFontMetrix                     -- ^ 'fmtx' (optional)
  | OTTDataForkFont                   -- ^ 'fond' (optional)
  | OTTFontProgram                    -- ^ 'fpgm' (optional)
  | OTTFontVariations                 -- ^ 'fvar' (optional)
  | OTTGridFittingAndScanConversion   -- ^ 'gasp' (optional)
  | OTTCIDMappings                    -- ^ 'gcid' (optional)
  | OOTGlyphDefinition                -- ^ 'GDEF' (optional)
  | RTTGlyphData                      -- ^ 'glyf' (required)
  | OOTGlyphPositioning               -- ^ 'GPOS' (optional)
  | OOTGlyphSubstitution              -- ^ 'GSUB' (optional)
  | OTTGlyphVariations                -- ^ 'gvar' (optional)
  | OTTHorizontalDeviceMetrics        -- ^ 'hdmx' (optional)
  | RTTFontHeader                     -- ^ 'head' (required)
  | RTTHorizontalHeader               -- ^ 'hhea' (required)
  | RTTHorizontalMetrics              -- ^ 'hmtx' (required)
  | OOTHorizontalMetricsVariations    -- ^ 'HVAR' (optional)
  | OOTJustification                  -- ^ 'JSTF' (optional)
  | OTTJustification                  -- ^ 'just' (optional)
  | OTTKerning                        -- ^ 'kern' (optional)
  | OTTExtendedKerning                -- ^ 'kerx' (optional)
  | OTTLigatureCaret                  -- ^ 'lcar' (optional)
  | RTTIndexToLocation                -- ^ 'loca' (required)
  | OTTLanguageTags                   -- ^ 'ltag' (optional)
  | OOTLinearThreshold                -- ^ 'LTSH' (optional)
  | OOTMathematicalTypesetting        -- ^ 'MATH' (optional)
  | RTTMaximumProfile                 -- ^ 'maxp' (required)
  | OOTMerge                          -- ^ 'MERG' (optional)
  | OTTMetadata                       -- ^ 'meta' (optional)
  | OTTGlyphMetamorphosis             -- ^ 'mort' (optional)
  | OTTExtendedGlyphMetamorphosis     -- ^ 'morx' (optional)
  | OOTMetricsVariations              -- ^ 'MVAR' (optional)
  | RTTNaming                         -- ^ 'name' (required)
  | OTTOpticalBounds                  -- ^ 'opbd' (optional)
  | OTTOS2                            -- ^ 'OS/2' (optional)
  | OOTPCL5                           -- ^ 'PCLT' (optional)
  | RTTPostScript                     -- ^ 'post' (required)
  | OTTControlValueProgram            -- ^ 'prep' (optional)
  | OTTGlyphProperties                -- ^ 'prop' (optional)
  | OTTStandardBitmapGraphics         -- ^ 'sbix' (optional)
  | OOTStyleAttributes                -- ^ 'STAT' (optional)
  | OOTSVG                            -- ^ 'SVG ' (optional)
  | OTTTracking                       -- ^ 'trak' (optional)
  | OOTVerticalDeviceMetrics          -- ^ 'VDMX' (optional)
  | OTTVerticalHeader                 -- ^ 'vhea' (optional)
  | OTTVerticalMetrics                -- ^ 'vmtx' (optional)
  | OOTVerticalOrigin                 -- ^ 'VORG' (optional)
  | OOTVerticalMetricsVariations      -- ^ 'VVAR' (optional)
  | OTTCrossReference                 -- ^ 'xref' (optional)
  | OTTGlyphsInformation              -- ^ 'Zapf' (optional)
  | OUnknownIdentifier ByteString
  deriving stock (Eq, Show)

{-|
Convert a table identifier to its 4-byte tag.

Inverse of 'toTableIdentifier'. Returns the standard ASCII tag for recognized
table types, or the original 4-byte identifier for 'OUnknownIdentifier' values.
-}
fromTableIdentifier :: TableIdentifier -> ByteString
fromTableIdentifier OTTAccentAttachment             = "acnt"
fromTableIdentifier OTTAnchorPointTable             = "ankr"
fromTableIdentifier OTTAxisVariation                = "avar"
fromTableIdentifier OOTBaseline                     = "BASE"
fromTableIdentifier OTTBitmapData                   = "bdat"
fromTableIdentifier OTTBitmapFontHeader             = "bhed"
fromTableIdentifier OTTBitmapLocation               = "bloc"
fromTableIdentifier OTTBaseline                     = "bsln"
fromTableIdentifier OOTColorBitmapData              = "CBDT"
fromTableIdentifier OOTColorBitmapLocation          = "CBLC"
fromTableIdentifier OOTCompactFontFormat            = "cff "
fromTableIdentifier RTTCharacterToGlyphMappin       = "cmap"
fromTableIdentifier OOTColor                        = "COLR"
fromTableIdentifier OOTColorPalette                 = "CPAL"
fromTableIdentifier OTTCVTVariations                = "cvar"
fromTableIdentifier OTTControlValue                 = "cvt "
fromTableIdentifier OOTDigitalSignature             = "DSIG"
fromTableIdentifier OOTEmbeddedBitmapData           = "EBDT"
fromTableIdentifier OOTEmbeddedBitmapLocation       = "EBLC"
fromTableIdentifier OTTEmbeddedBitmapScaling        = "EBSC"
fromTableIdentifier OTTFontDescriptors              = "fdsc"
fromTableIdentifier OTTFeatureName                  = "feat"
fromTableIdentifier OTTFontMetrix                   = "fmtx"
fromTableIdentifier OTTDataForkFont                 = "fond"
fromTableIdentifier OTTFontProgram                  = "fpgm"
fromTableIdentifier OTTFontVariations               = "fvar"
fromTableIdentifier OTTGridFittingAndScanConversion = "gasp"
fromTableIdentifier OTTCIDMappings                  = "gcid"
fromTableIdentifier OOTGlyphDefinition              = "GDEF"
fromTableIdentifier RTTGlyphData                    = "glyf"
fromTableIdentifier OOTGlyphPositioning             = "GPOS"
fromTableIdentifier OOTGlyphSubstitution            = "GSUB"
fromTableIdentifier OTTGlyphVariations              = "gvar"
fromTableIdentifier OTTHorizontalDeviceMetrics      = "hdmx"
fromTableIdentifier RTTFontHeader                   = "head"
fromTableIdentifier RTTHorizontalHeader             = "hhea"
fromTableIdentifier RTTHorizontalMetrics            = "hmtx"
fromTableIdentifier OOTHorizontalMetricsVariations  = "HVAR"
fromTableIdentifier OOTJustification                = "JSTF"
fromTableIdentifier OTTJustification                = "just"
fromTableIdentifier OTTKerning                      = "kern"
fromTableIdentifier OTTExtendedKerning              = "kerx"
fromTableIdentifier OTTLigatureCaret                = "lcar"
fromTableIdentifier RTTIndexToLocation              = "loca"
fromTableIdentifier OTTLanguageTags                 = "ltag"
fromTableIdentifier OOTLinearThreshold              = "LTSH"
fromTableIdentifier OOTMathematicalTypesetting      = "MATH"
fromTableIdentifier RTTMaximumProfile               = "maxp"
fromTableIdentifier OOTMerge                        = "MERG"
fromTableIdentifier OTTMetadata                     = "meta"
fromTableIdentifier OTTGlyphMetamorphosis           = "mort"
fromTableIdentifier OTTExtendedGlyphMetamorphosis   = "morx"
fromTableIdentifier OOTMetricsVariations            = "MVAR"
fromTableIdentifier RTTNaming                       = "name"
fromTableIdentifier OTTOpticalBounds                = "opbd"
fromTableIdentifier OTTOS2                          = "OS/2"
fromTableIdentifier OOTPCL5                         = "PCLT"
fromTableIdentifier RTTPostScript                   = "post"
fromTableIdentifier OTTControlValueProgram          = "prep"
fromTableIdentifier OTTGlyphProperties              = "prop"
fromTableIdentifier OTTStandardBitmapGraphics       = "sbix"
fromTableIdentifier OOTStyleAttributes              = "STAT"
fromTableIdentifier OOTSVG                          = "SVG "
fromTableIdentifier OTTTracking                     = "trak"
fromTableIdentifier OOTVerticalDeviceMetrics        = "VDMX"
fromTableIdentifier OTTVerticalHeader               = "vhea"
fromTableIdentifier OTTVerticalMetrics              = "vmtx"
fromTableIdentifier OOTVerticalOrigin               = "VORG"
fromTableIdentifier OOTVerticalMetricsVariations    = "VVAR"
fromTableIdentifier OTTCrossReference               = "xref"
fromTableIdentifier OTTGlyphsInformation            = "Zapf"
fromTableIdentifier (OUnknownIdentifier identifier) = BS.take 4 identifier

{-|
Convert a 4-byte tag to a table identifier.

Maps standard font table tags (e.g., "head", "glyf", "cmap", "GPOS") to their
corresponding 'TableIdentifier' constructors. Unrecognized tags are wrapped in
'OUnknownIdentifier'.

If the input is longer than 4 bytes, only the first 4 bytes are used. If
shorter, it's used as-is in 'OUnknownIdentifier'.

Inverse of 'fromTableIdentifier'.
-}
toTableIdentifier :: ByteString -> TableIdentifier
toTableIdentifier "acnt"     = OTTAccentAttachment
toTableIdentifier "ankr"     = OTTAnchorPointTable
toTableIdentifier "avar"     = OTTAxisVariation
toTableIdentifier "BASE"     = OOTBaseline
toTableIdentifier "bdat"     = OTTBitmapData
toTableIdentifier "bhed"     = OTTBitmapFontHeader
toTableIdentifier "bloc"     = OTTBitmapLocation
toTableIdentifier "bsln"     = OTTBaseline
toTableIdentifier "CBDT"     = OOTColorBitmapData
toTableIdentifier "CBLC"     = OOTColorBitmapLocation
toTableIdentifier "cff "     = OOTCompactFontFormat
toTableIdentifier "cmap"     = RTTCharacterToGlyphMappin
toTableIdentifier "COLR"     = OOTColor
toTableIdentifier "CPAL"     = OOTColorPalette
toTableIdentifier "cvar"     = OTTCVTVariations
toTableIdentifier "cvt "     = OTTControlValue
toTableIdentifier "DSIG"     = OOTDigitalSignature
toTableIdentifier "EBDT"     = OOTEmbeddedBitmapData
toTableIdentifier "EBLC"     = OOTEmbeddedBitmapLocation
toTableIdentifier "EBSC"     = OTTEmbeddedBitmapScaling
toTableIdentifier "fdsc"     = OTTFontDescriptors
toTableIdentifier "feat"     = OTTFeatureName
toTableIdentifier "fmtx"     = OTTFontMetrix
toTableIdentifier "fond"     = OTTDataForkFont
toTableIdentifier "fpgm"     = OTTFontProgram
toTableIdentifier "fvar"     = OTTFontVariations
toTableIdentifier "gasp"     = OTTGridFittingAndScanConversion
toTableIdentifier "gcid"     = OTTCIDMappings
toTableIdentifier "GDEF"     = OOTGlyphDefinition
toTableIdentifier "glyf"     = RTTGlyphData
toTableIdentifier "GPOS"     = OOTGlyphPositioning
toTableIdentifier "GSUB"     = OOTGlyphSubstitution
toTableIdentifier "gvar"     = OTTGlyphVariations
toTableIdentifier "hdmx"     = OTTHorizontalDeviceMetrics
toTableIdentifier "head"     = RTTFontHeader
toTableIdentifier "hhea"     = RTTHorizontalHeader
toTableIdentifier "hmtx"     = RTTHorizontalMetrics
toTableIdentifier "HVAR"     = OOTHorizontalMetricsVariations
toTableIdentifier "JSTF"     = OOTJustification
toTableIdentifier "just"     = OTTJustification
toTableIdentifier "kern"     = OTTKerning
toTableIdentifier "kerx"     = OTTExtendedKerning
toTableIdentifier "lcar"     = OTTLigatureCaret
toTableIdentifier "loca"     = RTTIndexToLocation
toTableIdentifier "ltag"     = OTTLanguageTags
toTableIdentifier "LTSH"     = OOTLinearThreshold
toTableIdentifier "MATH"     = OOTMathematicalTypesetting
toTableIdentifier "maxp"     = RTTMaximumProfile
toTableIdentifier "MERG"     = OOTMerge
toTableIdentifier "meta"     = OTTMetadata
toTableIdentifier "mort"     = OTTGlyphMetamorphosis
toTableIdentifier "morx"     = OTTExtendedGlyphMetamorphosis
toTableIdentifier "MVAR"     = OOTMetricsVariations
toTableIdentifier "name"     = RTTNaming
toTableIdentifier "opbd"     = OTTOpticalBounds
toTableIdentifier "OS/2"     = OTTOS2
toTableIdentifier "PCLT"     = OOTPCL5
toTableIdentifier "post"     = RTTPostScript
toTableIdentifier "prep"     = OTTControlValueProgram
toTableIdentifier "prop"     = OTTGlyphProperties
toTableIdentifier "sbix"     = OTTStandardBitmapGraphics
toTableIdentifier "STAT"     = OOTStyleAttributes
toTableIdentifier "SVG "     = OOTSVG
toTableIdentifier "trak"     = OTTTracking
toTableIdentifier "VDMX"     = OOTVerticalDeviceMetrics
toTableIdentifier "vhea"     = OTTVerticalHeader
toTableIdentifier "vmtx"     = OTTVerticalMetrics
toTableIdentifier "VORG"     = OOTVerticalOrigin
toTableIdentifier "VVAR"     = OOTVerticalMetricsVariations
toTableIdentifier "xref"     = OTTCrossReference
toTableIdentifier "Zapf"     = OTTGlyphsInformation
toTableIdentifier identifier = OUnknownIdentifier (BS.take 4 identifier)

{-|
Assign a priority level to each table identifier for ordering.

Returns an integer representing the logical processing order of tables:

* 0-1: Core glyph data tables (glyf, loca)
* 10-15: Font header and metadata tables (head, maxp, hhea, etc.)
* 21-22: Character mapping tables (cmap, gcid)
* 30-36: Metrics and kerning tables (hmtx, vmtx, hdmx, kern, etc.)
* 40-45: Variation tables (gvar, cvar, avar, etc.)
* 50-53: Hinting and grid-fitting tables (fpgm, prep, cvt, gasp)
* 60-64: OpenType layout tables (GDEF, GPOS, GSUB, BASE, JSTF)
* 70-73: Naming and metadata (name, meta, post, ltag)
* 80-88: Apple AAT layout tables (mort, morx, feat, etc.)
* 90-97: Bitmap and color tables (CBDT, EBDT, sbix, SVG, etc.)
* 100+: Miscellaneous and unknown tables

This ordering ensures tables are processed in dependency order when iterating
through a font's table directory.
-}
identifierLevel :: TableIdentifier -> Int
identifierLevel RTTGlyphData       = 0  -- 'glyf'
identifierLevel RTTIndexToLocation = 1  -- 'loca'

identifierLevel RTTFontHeader       = 10 -- 'head'
identifierLevel RTTMaximumProfile   = 11 -- 'maxp'
identifierLevel RTTHorizontalHeader = 12 -- 'hhea'
identifierLevel OTTVerticalHeader   = 13 -- 'vhea'
identifierLevel OTTOS2              = 14 -- 'OS/2'
identifierLevel OTTFontVariations   = 15 -- 'fvar'

identifierLevel RTTCharacterToGlyphMappin = 21 -- 'cmap'
identifierLevel OTTCIDMappings            = 22 -- 'gcid' / 'ltag'

identifierLevel RTTHorizontalMetrics       = 30 -- 'hmtx'
identifierLevel OTTVerticalMetrics         = 31 -- 'vmtx'
identifierLevel OTTHorizontalDeviceMetrics = 32 -- 'hdmx'
identifierLevel OOTVerticalDeviceMetrics   = 33 -- 'vdmx'
identifierLevel OTTKerning                 = 34 -- 'kern'
identifierLevel OTTExtendedKerning         = 35 -- 'kerx'
identifierLevel OTTAnchorPointTable        = 36 -- 'ankr'

identifierLevel OTTGlyphVariations             = 40 -- 'gvar'
identifierLevel OTTCVTVariations               = 41 -- 'cvar'
identifierLevel OTTAxisVariation               = 42 -- 'avar'
identifierLevel OOTMetricsVariations           = 43 -- 'MVAR'
identifierLevel OOTHorizontalMetricsVariations = 44 -- 'HVAR'
identifierLevel OOTVerticalMetricsVariations   = 45 -- 'VVAR'

identifierLevel OTTFontProgram                  = 50 -- 'fpgm'
identifierLevel OTTControlValueProgram          = 51 -- 'prep'
identifierLevel OTTControlValue                 = 52 -- 'cvt '
identifierLevel OTTGridFittingAndScanConversion = 53 -- 'gasp'

identifierLevel OOTGlyphDefinition   = 60 -- 'GDEF'
identifierLevel OOTGlyphPositioning  = 61 -- 'GPOS'
identifierLevel OOTGlyphSubstitution = 62 -- 'GSUB'
identifierLevel OOTBaseline          = 63 -- 'BASE'
identifierLevel OOTJustification     = 64 -- 'JSTF'

identifierLevel RTTNaming       = 70 -- 'name'
identifierLevel OTTMetadata     = 71 -- 'meta'
identifierLevel RTTPostScript   = 72 -- 'post'
identifierLevel OTTLanguageTags = 73 -- 'ltag'

identifierLevel OTTGlyphMetamorphosis         = 80 -- 'mort'
identifierLevel OTTExtendedGlyphMetamorphosis = 81 -- 'morx'
identifierLevel OTTFeatureName                = 82 -- 'feat'
identifierLevel OTTGlyphProperties            = 83 -- 'prop'
identifierLevel OTTJustification              = 84 -- 'just'
identifierLevel OTTTracking                   = 85 -- 'trak'
identifierLevel OTTOpticalBounds              = 86 -- 'opbd'
identifierLevel OTTBaseline                   = 87 -- 'bsln'
identifierLevel OTTLigatureCaret              = 88 -- 'lcar'

identifierLevel OOTColorBitmapData        = 90 -- 'CBDT'
identifierLevel OOTColorBitmapLocation    = 91 -- 'CBLC'
identifierLevel OOTEmbeddedBitmapData     = 92 -- 'EBDT'
identifierLevel OOTEmbeddedBitmapLocation = 93 -- 'EBLC'
identifierLevel OTTBitmapData             = 94 -- 'bdat'
identifierLevel OTTBitmapLocation         = 95 -- 'bloc'
identifierLevel OTTStandardBitmapGraphics = 96 -- 'sbix'
identifierLevel OOTSVG                    = 97 -- 'SVG '

identifierLevel OTTCrossReference          = 100 -- 'xref'
identifierLevel OTTGlyphsInformation       = 101 -- 'Zapf'
identifierLevel OOTDigitalSignature        = 102 -- 'DSIG'
identifierLevel OOTMathematicalTypesetting = 103 -- 'MATH'
identifierLevel OOTCompactFontFormat       = 104 -- 'CFF '
identifierLevel OTTDataForkFont            = 105

identifierLevel OTTAccentAttachment      = 106 -- 'acnt'
identifierLevel OTTBitmapFontHeader      = 107 -- 'bhed'
identifierLevel OOTColor                 = 108 -- 'COLR'
identifierLevel OOTColorPalette          = 109 -- 'CPAL'
identifierLevel OTTEmbeddedBitmapScaling = 110 -- 'EBSC'
identifierLevel OTTFontDescriptors       = 111 -- 'fdsc'
identifierLevel OTTFontMetrix            = 112 -- 'fmtx'
identifierLevel OOTLinearThreshold       = 113 -- 'LTSH'
identifierLevel OOTMerge                 = 114 -- 'MERG'
identifierLevel OOTPCL5                  = 115 -- 'PCLT'
identifierLevel OOTStyleAttributes       = 116 -- 'STAT'
identifierLevel OOTVerticalOrigin        = 117 -- 'VORG'

identifierLevel (OUnknownIdentifier _) = 1000

{-|
Ordering instance based on logical table dependencies.

Tables are ordered by their 'identifierLevel', not alphabetically by tag. This
ensures tables are processed in the correct dependency order:

1. Glyph data and indices (required for all other tables)
2. Font headers and basic metrics
3. Character-to-glyph mappings
4. Advanced metrics and kerning
5. Layout and typography features
6. Metadata and auxiliary tables

This ordering is particularly useful when iterating through font tables for
validation, processing, or serialization.
-}
instance Ord TableIdentifier where
  compare :: TableIdentifier -> TableIdentifier -> Ordering
  compare id1 id2 = compare (identifierLevel id1) (identifierLevel id2)
