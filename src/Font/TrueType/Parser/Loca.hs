{-|
Parse the TrueType "loca" (Index to Location) table.

The loca table stores the offsets to the locations of the glyphs in the font,
relative to the beginning of the glyph data table. By indexing into the loca
table, you can find the starting position and length of each glyph's data.

The table comes in two formats:
- Short format (indexToLocFormat = 0): Uses 16-bit offsets that must be
  multiplied by 2 to get the actual offset
- Long format (indexToLocFormat = 1): Uses 32-bit offsets that are the actual
  byte offsets

Reference: https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6loca.html
-}
module Font.TrueType.Parser.Loca
  ( LocaFormat(..)
  , fromIndexToLocFormat
  , locaP
  ) where

import Control.Monad (replicateM)

import Data.Binary.Get (getWord16be, getWord32be)
import Data.Binary.Parser (Get, label)
import Data.Int (Int16)
import Data.Kind (Type)
import Data.Word (Word32)

import Font.TrueType.FontTable.LocationTable (LocationTable (LocationTable))

{-|
Format of the loca table, determined by the indexToLocFormat field in the head table.
-}
type LocaFormat :: Type
data LocaFormat
  = ShortFormat  -- ^ 0: Short offsets (Offset16), actual offset = value * 2
  | LongFormat   -- ^ 1: Long offsets (Offset32), actual offset = value
  deriving stock (Eq, Show)

{-|
Convert an Int16 from the head table's indexToLocFormat to LocaFormat.
-}
fromIndexToLocFormat :: Int16 -> LocaFormat
fromIndexToLocFormat 0 = ShortFormat
fromIndexToLocFormat _ = LongFormat

{-|
Parse a short format loca entry (16-bit offset).

Reads a 16-bit unsigned integer and multiplies it by 2 to get the actual offset.
-}
shortOffsetP :: Get Word32
shortOffsetP = do
  offset16 <- getWord16be
  return (fromIntegral offset16 * 2)

{-|
Parse a long format loca entry (32-bit offset).

Reads a 32-bit unsigned integer which is the actual offset.
-}
longOffsetP :: Get Word32
longOffsetP = getWord32be

{-|
Parse the complete loca (Index to Location) table.

The number of entries in the loca table is numGlyphs + 1. The last entry points
to the end of the last glyph's data, which allows calculating each glyph's
length by subtracting consecutive offsets.

Parameters:
- format: The format of the table (ShortFormat or LongFormat)
- numGlyphs: The number of glyphs in the font (from maxp table)

Returns:
- A LocationTable containing all glyph offsets
-}
locaP :: LocaFormat -> Int -> Get LocationTable
locaP format numGlyphs = label "loca" $ do
  let numEntries = numGlyphs + 1
      offsetParser = case format of
        ShortFormat -> shortOffsetP
        LongFormat  -> longOffsetP
  
  offsets <- replicateM numEntries offsetParser
  return (LocationTable offsets)
