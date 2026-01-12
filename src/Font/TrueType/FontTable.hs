{-|
TrueType font table structures and serialization.

Defines the structure of TrueType font tables including the head (font header)
table with metadata such as version, timestamps, and bounding boxes. Provides
utilities for serializing these structures back to binary format.
-}
module Font.TrueType.FontTable
  ( FontTable(FTRaw, FTHead, FTLoca, FTGlyf)
  , fromFontTable
  ) where

import Data.ByteString (ByteString)
import Data.Kind (Type)

import Font.TrueType.FontTable.GlyphTable (GlyphTable, fromGlyphTable)
import Font.TrueType.FontTable.HeadTable (HeadTable, fromHeadTable)
import Font.TrueType.FontTable.LocationTable (LocationTable, fromLocationTable)

{-|
TrueType font table representation.
-}
type FontTable :: Type
data FontTable
  = FTRaw ByteString -- ^ Raw table data (for unparsed tables)
  | FTLoca LocationTable -- ^ 'loca', location table
  | FTGlyf GlyphTable -- ^ 'glyf', glyph table
  | FTHead HeadTable -- ^ 'head', font header table
  deriving stock (Eq, Show)

{-|
Extract raw binary data from a FontTable.
-}
fromFontTable :: FontTable -> ByteString
fromFontTable (FTRaw raw)    = raw
fromFontTable (FTLoca loca)  = fromLocationTable loca
fromFontTable (FTGlyf glyf)  = fromGlyphTable glyf
fromFontTable (FTHead headT) = fromHeadTable headT
