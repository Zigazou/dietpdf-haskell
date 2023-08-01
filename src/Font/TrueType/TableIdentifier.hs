{- |
This module has all the standard table identifier for True Type and Open Type
file formats.

Source:
- https://developer.apple.com/fonts/TrueType-Reference-Manual/
- https://learn.microsoft.com/en-us/typography/opentype/spec/
-}
module Font.TrueType.TableIdentifier
  ( TableIdentifier(..)
  , fromTableIdentifier
  , toTableIdentifier
  ) where

import qualified Data.ByteString               as BS

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
  | OTTLanguageTags                   -- ^ 'ltag' (optiional)
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
  | OUnknownIdentifier BS.ByteString
  deriving stock (Eq, Show)

fromTableIdentifier :: TableIdentifier -> BS.ByteString
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

toTableIdentifier :: BS.ByteString -> TableIdentifier
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
