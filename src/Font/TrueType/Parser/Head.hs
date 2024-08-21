module Font.TrueType.Parser.Head
  ( headP
  ) where

import Data.Binary.Get (getInt16be, getInt64be, getWord16be, getWord32be)
import Data.Binary.Parser (Get)
import Data.Binary.Parser.Word8 (string)

import Font.TrueType.FontTable
    ( Fixed (Fixed)
    , Head (Head, hCheckSumAdjustment, hCreated, hFlags, hFontDirectionHint, hFontRevision, hGlyphDataFormat, hIndexToLocFormat, hLowestRecPPEM, hMacStyle, hMagicNumber, hModified, hUnitsPerEm, hVersion, hXMax, hXMin, hYMax, hYMin)
    )

getFixed :: Get Fixed
getFixed = Fixed <$> getWord16be <*> getWord16be

headP :: Get Head
headP = do
  version            <- getFixed
  fontRevision       <- getFixed
  checkSumAdjustment <- getWord32be
  string "\x5F\x0F\x3C\xF5"
  flags             <- getWord16be
  unitsPerEm        <- getWord16be
  created           <- getInt64be
  modified          <- getInt64be
  xMin              <- getInt16be
  yMin              <- getInt16be
  xMax              <- getInt16be
  yMax              <- getInt16be
  macStyle          <- getWord16be
  lowestRecPPEM     <- getWord16be
  fontDirectionHint <- getInt16be
  indexToLocFormat  <- getInt16be
  glyphDataFormat   <- getInt16be

  return Head { hVersion            = version
              , hFontRevision       = fontRevision
              , hCheckSumAdjustment = checkSumAdjustment
              , hMagicNumber        = 0x5F0F3CF5
              , hFlags              = flags
              , hUnitsPerEm         = unitsPerEm
              , hCreated            = created
              , hModified           = modified
              , hXMin               = xMin
              , hYMin               = yMin
              , hXMax               = xMax
              , hYMax               = yMax
              , hMacStyle           = macStyle
              , hLowestRecPPEM      = lowestRecPPEM
              , hFontDirectionHint  = fontDirectionHint
              , hIndexToLocFormat   = indexToLocFormat
              , hGlyphDataFormat    = glyphDataFormat
              }
