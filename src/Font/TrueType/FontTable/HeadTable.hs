module Font.TrueType.FontTable.HeadTable
  ( HeadTable
    ( HeadTable
    , hVersion
    , hFontRevision
    , hCheckSumAdjustment
    , hMagicNumber
    , hFlags
    , hUnitsPerEm
    , hCreated
    , hModified
    , hXMin
    , hYMin
    , hXMax
    , hYMax
    , hMacStyle
    , hLowestRecPPEM
    , hFontDirectionHint
    , hIndexToLocFormat
    , hGlyphDataFormat
    )
  , fromHeadTable
  , Fixed(Fixed)
  ) where

import Data.Binary.Put
  (PutM, putInt16be, putInt64be, putWord16be, putWord32be, runPut)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Int (Int16, Int64)
import Data.Kind (Type)
import Data.Word (Word16, Word32)

{-|
Fixed-point number in 16.16 format.
-}
type Fixed :: Type
data Fixed = Fixed Word16 Word16
  deriving stock (Eq, Show)

{-|
TrueType font header.
-}
type HeadTable :: Type
data HeadTable = HeadTable
  { -- | Version, 0x00010000 if (version 1.0)
    hVersion            :: Fixed
  , -- | Font revision, set by font manufacturer
    hFontRevision       :: Fixed
  , -- | To compute: set it to 0, calculate the checksum for the 'head' table
    --   and put it in the table directory, sum the entire font as a uint32_t,
    --   then store 0xB1B0AFBA - sum. (The checksum for the 'head' table will
    --   be wrong as a result. That is OK; do not reset it.)
    hCheckSumAdjustment :: Word32
  , -- | Magic number, set to 0x5F0F3CF5
    hMagicNumber        :: Word32
  , -- | Various flags
    hFlags              :: Word16
  , -- | Units per em, range from 64 to 16384
    hUnitsPerEm         :: Word16
  , -- | International date of creation
    hCreated            :: Int64
  , -- | International date of modification
    hModified           :: Int64
  , -- | for all glyph bounding boxes
    hXMin               :: Int16
  , -- | for all glyph bounding boxes
    hYMin               :: Int16
  , -- | for all glyph bounding boxes
    hXMax               :: Int16
  , -- | for all glyph bounding boxes
    hYMax               :: Int16
  , -- | Mac style, bold, underline, outline, shadow, condensed, extended
    hMacStyle           :: Word16
  , -- | Smallest readable size in pixels
    hLowestRecPPEM      :: Word16
  , -- | Font direction hint (from -2 to 2)
    hFontDirectionHint  :: Int16
  , -- | Short or long offsets (0 or 1)
    hIndexToLocFormat   :: Int16
  , -- | Glyph data format (0=current format)
    hGlyphDataFormat    :: Int16
  }
  deriving stock (Eq, Show)

{-|
Serialize a Fixed-point number to binary (16.16 format).

Writes two 16-bit big-endian words representing the integer and fractional
parts.
-}
putFixed :: Fixed -> PutM ()
putFixed (Fixed ah al) = putWord16be ah >> putWord16be al

{-|
Serialize a HeadTable record to binary bytes.

Converts all fields of a 'HeadTable' record into the binary format specified by the
TrueType font specification, returning a strict ByteString.
-}
fromHeadTable :: HeadTable -> ByteString
fromHeadTable fontHead = BSL.toStrict $ runPut $ do
  putFixed (hVersion fontHead)
  putFixed (hFontRevision fontHead)
  putWord32be (hCheckSumAdjustment fontHead)
  putWord32be (hMagicNumber fontHead)
  putWord16be (hFlags fontHead)
  putWord16be (hUnitsPerEm fontHead)
  putInt64be (hCreated fontHead)
  putInt64be (hModified fontHead)
  putInt16be (hXMin fontHead)
  putInt16be (hYMin fontHead)
  putInt16be (hXMax fontHead)
  putInt16be (hYMax fontHead)
  putWord16be (hMacStyle fontHead)
  putWord16be (hLowestRecPPEM fontHead)
  putInt16be (hFontDirectionHint fontHead)
  putInt16be (hIndexToLocFormat fontHead)
  putInt16be (hGlyphDataFormat fontHead)
