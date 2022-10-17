{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}

module Font.TrueType.FontDirectory
  ( TableIdentifier(..)
  , ScalerType
    ( FontTrueTypeTrue
    , FontTrueType00010000
    , FontOldStyleSFNT
    , FontOpenType
    )
  , OffsetSubtable(..)
  , TableEntry
  , TableDirectory
  , FontDirectory(..)
  , calcTableChecksum
  ) where

import qualified Data.ByteString               as BS
import           Data.Bits                      ( (.&.) )
import           Data.Binary.Parser             ( parseOnly
                                                , Get
                                                )
import           Data.Binary.Parser.Word8       ( anyWord8 )
import           Control.Applicative            ( (<|>) )
import           Util.Array                     ( Array )

data TableIdentifier
  = OAccentAttachment -- ^ 'acnt' (optional)
  | OAnchorPointTable -- ^ 'ankr' (optional)
  | OAxisVariation -- ^ 'avar' (optional)
  | OBitmapData -- ^ 'bdat' (optional)
  | OBitmapFontHeader -- ^ 'bhed' (optional)
  | OBitmapLocation -- ^ 'bloc' (optional)
  | OBaseline -- ^ 'bsln' (optional)
  | RCharacterToGlyphMappin -- ^ 'cmap' (required)
  | OCVTVariations -- ^ 'cvar' (optional)
  | OControlValue -- ^ 'cvt ' (optional)
  | OEmbeddedBitmapScalerType -- ^ 'EBSC' (optional)
  | OFontDescriptors -- ^ 'fdsc' (optional)
  | OFeatureName -- ^ 'feat' (optional)
  | OFontMetrix -- ^ 'fmtx' (optional)
  | ODataForkFont -- ^ 'fond' (optional)
  | OFontProgram -- ^ 'fpgm' (optional)
  | OFontVariations -- ^ 'fvar' (optional)
  | OGrayScaleParameters -- ^ 'gasp' (optional)
  | OCIDMappings -- ^ 'gcid' (optional)
  | RGlyphData -- ^ 'glyf' (required)
  | OGlyphVariations -- ^ 'gvar' (optional)
  | OHorizontalDeviceMetrics -- ^ 'hdmx' (optional)
  | RFontHeader -- ^ 'head' (required)
  | RHorizontalHeader -- ^ 'hhea' (required)
  | RHorizontalMetrics -- ^ 'hmtx' (required)
  | OJustification -- ^ 'just' (optional)
  | OKerning -- ^ 'kern' (optional)
  | OExtendedKerning -- ^ 'kerx' (optional)
  | OLigatureCaret -- ^ 'lcar' (optional)
  | RIndexToLocation -- ^ 'loca' (required)
  | OLanguageTags -- ^ 'ltag' (optiional)
  | RMaximumProfile -- ^ 'maxp' (required)
  | OMetadata -- ^ 'meta' (optional)
  | OGlyphMetamorphosis -- ^ 'mort' (optional)
  | OExtendedGlyphMetamorphosis -- ^ 'morx' (optional)
  | RNaming -- ^ 'name' (required)
  | OOpticalBounds -- ^ 'opbd' (optional)
  | OOS2 -- ^ 'OS/2' (optional)
  | RPostScript -- ^ 'post' (required)
  | OControlValueProgram -- ^ 'prep' (optional)
  | OGlyphProperties -- ^ 'prop' (optional)
  | OBitmapFontStrikes -- ^ 'sbix' (optional)
  | OTracking -- ^ 'trak' (optional)
  | OVerticalHeader -- ^ 'vhea' (optional)
  | OVerticalMetrics -- ^ 'vmtx' (optional)
  | OCrossReference -- ^ 'xref' (optional)
  | OGlyphsInformation -- ^ 'Zapf' (optional)
  | OUnknownIdentifier BS.ByteString
  deriving stock (Eq, Show)

getUInt32 :: Get Int
getUInt32 =
  (do
      a <- fromIntegral <$> anyWord8
      b <- fromIntegral <$> anyWord8
      c <- fromIntegral <$> anyWord8
      d <- fromIntegral <$> anyWord8
      return (a * 16777216 + b * 65536 + c * 256 + d)
    )
    <|> return 0

calcTableChecksum :: BS.ByteString -> Int
calcTableChecksum bytes =
  foldr (\long acc -> (acc + long) .&. 0xFFFFFFFF) 0 (parseOnly getUInt32 bytes)

data ScalerType =
  FontTrueTypeTrue | FontTrueType00010000 | FontOldStyleSFNT | FontOpenType
  deriving stock (Eq, Show)

data OffsetSubtable = OffsetSubtable
  { osScalerType    :: ScalerType -- ^ Scaler to be used to rasterize this font
  , osNumTables     :: Int -- ^ Number of tables
  , osSearchRange   :: Int -- ^ Search range (max power of 2 <= numTables)*16
  , osEntrySelector :: Int -- ^ log2(maximum power of 2 <= numTables)
  , osRangeShift    :: Int -- ^ numTables*16-searchRange
  }
  deriving stock Show

data TableEntry = TableEntry
  { tdTag      :: TableIdentifier -- ^ 4-byte identifier
  , tdChecksum :: Int -- ^ Checksum for this table
  , tdOffset   :: Int -- ^ Offset from beginning of sfnt
  , tdLength   :: Int -- ^ Length of this table in byte (actual length)
  }
  deriving stock Show

type TableDirectory = Array TableEntry

data FontDirectory = FontDirectory
  { fdOffsetSubtable :: OffsetSubtable
  , fdTableDirectory :: TableDirectory
  }
