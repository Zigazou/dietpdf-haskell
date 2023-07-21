{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}

module Font.TrueType.FontDirectory
  ( TableDirectory
  , FontDirectory(FontDirectory, fdOffsetSubtable, fdTableDirectory)
  , OffsetSubtable
    ( OffsetSubtable
    , osScalerType
    , osNumTables
    , osSearchRange
    , osEntrySelector
    , osRangeShift
    )
  , TableEntry(TableEntry, teTag, teChecksum, teOffset, teLength, teData)
  , loadContent
  , calcChecksum
  , calcTableChecksum
  ) where

import qualified Data.ByteString               as BS
import           Data.Either                    ( fromRight )
import           Data.Binary.Parser             ( parseOnly
                                                , many'
                                                , getWord32be
                                                )
import           Util.Array                     ( Array )
import           Data.Binary                    ( Word16
                                                , Word32
                                                )
import           Font.TrueType.FontTable        ( FontTable(FTRaw, FTHead)
                                                , fromHead
                                                , Head(hCheckSumAdjustment)
                                                )
import           Font.TrueType.TableIdentifier  ( TableIdentifier(RTTFontHeader)
                                                )
import           Font.TrueType.ScalerType       ( ScalerType )
import           Font.TrueType.Parser.Head      ( headP )
import           Data.Bits                      ( shiftL )

calcChecksum :: BS.ByteString -> Word32
calcChecksum raw =
  foldr (+) lastValue . fromRight [] . parseOnly (many' getWord32be) $ raw
 where
  lastValue :: Word32
  lastValue =
    case BS.unpack (BS.drop (BS.length raw - (BS.length raw `rem` 4)) raw) of
      []     -> 0
      [a]    -> fromIntegral a `shiftL` 24
      [a, b] -> (fromIntegral a `shiftL` 24) + (fromIntegral b `shiftL` 16)
      [a, b, c] ->
        (fromIntegral a `shiftL` 24)
          + (fromIntegral b `shiftL` 16)
          + (fromIntegral c `shiftL` 8)
      _anyError -> 0

data OffsetSubtable = OffsetSubtable
  { osScalerType    :: ScalerType -- ^ Scaler to be used to rasterize this font
  , osNumTables     :: Word16 -- ^ Number of tables
  , osSearchRange   :: Word16 -- ^ Search range (max power of 2 <= numTables)*16
  , osEntrySelector :: Word16 -- ^ log2(maximum power of 2 <= numTables)
  , osRangeShift    :: Word16 -- ^ numTables*16-searchRange
  }
  deriving stock (Eq, Show)

data TableEntry = TableEntry
  { teTag      :: TableIdentifier -- ^ 4-byte identifier
  , teChecksum :: Word32 -- ^ Checksum for this table
  , teOffset   :: Word32 -- ^ Offset from beginning of sfnt
  , teLength   :: Word32 -- ^ Length of this table in byte (actual length)
  , teData     :: FontTable -- ^ Raw table data
  }
  deriving stock (Eq, Show)

getBytes :: BS.ByteString -> TableEntry -> BS.ByteString
getBytes bytes entry = BS.take
  (fromIntegral $ teLength entry)
  (BS.drop (fromIntegral $ teOffset entry) bytes)

loadContent :: BS.ByteString -> TableEntry -> TableEntry
loadContent bytes entry@TableEntry { teTag = RTTFontHeader } =
  let raw = getBytes bytes entry
  in  case parseOnly headP raw of
        Left  _      -> entry { teData = FTRaw raw }
        Right teHead -> entry { teData = FTHead teHead }
loadContent bytes entry = entry { teData = FTRaw (getBytes bytes entry) }

calcTableChecksum :: TableEntry -> Word32
calcTableChecksum TableEntry { teData = FTRaw raw } = calcChecksum raw
calcTableChecksum TableEntry { teData = FTHead fontHead } =
  calcChecksum (fromHead fontHead { hCheckSumAdjustment = 0 })

type TableDirectory = Array TableEntry

data FontDirectory = FontDirectory
  { fdOffsetSubtable :: OffsetSubtable
  , fdTableDirectory :: TableDirectory
  }
  deriving stock (Eq, Show)
