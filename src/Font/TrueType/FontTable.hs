{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}

module Font.TrueType.FontTable
  ( FontTable(FTRaw, FTHead)
  , Head
    ( Head
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
  , fromHead
  , Fixed(Fixed)
  ) where

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BSL
import           Data.Word                      ( Word16
                                                , Word32
                                                )
import           Data.Int                       ( Int16
                                                , Int64
                                                )
import           Data.Binary.Put                ( PutM
                                                , putWord16be
                                                , putWord32be
                                                , putInt64be
                                                , putInt16be
                                                , runPut
                                                )

data Fixed = Fixed Word16 Word16
  deriving stock (Eq, Show)

data Head = Head
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

putFixed :: Fixed -> PutM ()
putFixed (Fixed ah al) = putWord16be ah >> putWord16be al

fromHead :: Head -> BS.ByteString
fromHead fontHead = BSL.toStrict $ runPut $ do
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

data FontTable
  = FTRaw BS.ByteString
  | FTHead Head
  deriving stock (Eq, Show)
