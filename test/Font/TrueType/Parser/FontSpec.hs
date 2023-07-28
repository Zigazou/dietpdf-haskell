module Font.TrueType.Parser.FontSpec
  ( spec
  ) where

import           Test.Hspec                     ( describe
                                                , Spec
                                                , shouldBe
                                                , it
                                                )
import qualified Data.ByteString               as BS
import           Data.Word                      ( Word32 )
import           Font.TrueType.FontDirectory    ( FontDirectory
                                                  ( FontDirectory
                                                  , fdOffsetSubtable
                                                  , fdTableDirectory
                                                  )
                                                , OffsetSubtable
                                                  ( OffsetSubtable
                                                  , osScalerType
                                                  , osNumTables
                                                  , osSearchRange
                                                  , osEntrySelector
                                                  , osRangeShift
                                                  )
                                                , TableEntry
                                                  ( TableEntry
                                                  , teTag
                                                  , teChecksum
                                                  , teOffset
                                                  , teLength
                                                  , teData
                                                  )
                                                , loadContent
                                                , calcChecksum
                                                , calcTableChecksum
                                                )
import           Font.TrueType.Parser.Font      ( ttfParse )
import           Font.TrueType.ScalerType       ( ScalerType
                                                  ( FontTrueType00010000
                                                  )
                                                )
import           Font.TrueType.TableIdentifier  ( toTableIdentifier )
import           Control.Monad                  ( forM_ )
import           Util.Array                     ( mkArray )
import           Font.TrueType.FontTable        ( FontTable(FTRaw) )

fontExamples :: [(FilePath, FontDirectory)]
fontExamples =
  [ ( "Roboto/Roboto-Regular.ttf"
    , FontDirectory
      { fdOffsetSubtable = OffsetSubtable { osScalerType = FontTrueType00010000
                                          , osNumTables     = 18
                                          , osSearchRange   = 256
                                          , osEntrySelector = 4
                                          , osRangeShift    = 32
                                          }
      , fdTableDirectory = mkArray
                             [ TableEntry { teTag = toTableIdentifier "GDEF"
                                          , teChecksum = 0xB442B082
                                          , teOffset   = 0x00021B84
                                          , teLength   = 0x00000262
                                          , teData     = FTRaw ""
                                          }
                             , TableEntry { teTag = toTableIdentifier "GPOS"
                                          , teChecksum = 0xCB6D3F36
                                          , teOffset   = 0x00021DE8
                                          , teLength   = 0x00005DCC
                                          , teData     = FTRaw ""
                                          }
                             , TableEntry { teTag = toTableIdentifier "GSUB"
                                          , teChecksum = 0x7A818577
                                          , teOffset   = 0x00027BB4
                                          , teLength   = 0x00001590
                                          , teData     = FTRaw ""
                                          }
                             , TableEntry { teTag = toTableIdentifier "OS/2"
                                          , teChecksum = 0x9782B1A8
                                          , teOffset   = 0x0002096C
                                          , teLength   = 0x00000060
                                          , teData     = FTRaw ""
                                          }
                             , TableEntry { teTag = toTableIdentifier "cmap"
                                          , teChecksum = 0xC6EE516D
                                          , teOffset   = 0x00020EE4
                                          , teLength   = 0x00000682
                                          , teData     = FTRaw ""
                                          }
                             , TableEntry { teTag = toTableIdentifier "cvt "
                                          , teChecksum = 0x2BA8079D
                                          , teOffset   = 0x00021870
                                          , teLength   = 0x00000054
                                          , teData     = FTRaw ""
                                          }
                             , TableEntry { teTag = toTableIdentifier "fpgm"
                                          , teChecksum = 0x77F860AB
                                          , teOffset   = 0x00021568
                                          , teLength   = 0x000001BC
                                          , teData     = FTRaw ""
                                          }
                             , TableEntry { teTag = toTableIdentifier "gasp"
                                          , teChecksum = 0x00080013
                                          , teOffset   = 0x00021B78
                                          , teLength   = 0x0000000C
                                          , teData     = FTRaw ""
                                          }
                             , TableEntry { teTag = toTableIdentifier "glyf"
                                          , teChecksum = 0x26BA0BF4
                                          , teOffset   = 0x0000012C
                                          , teLength   = 0x0001E96C
                                          , teData     = FTRaw ""
                                          }
                             , TableEntry { teTag = toTableIdentifier "hdmx"
                                          , teChecksum = 0x557A607A
                                          , teOffset   = 0x000209CC
                                          , teLength   = 0x00000518
                                          , teData     = FTRaw ""
                                          }
                             , TableEntry { teTag = toTableIdentifier "head"
                                          , teChecksum = 0xFC6AD27A
                                          , teOffset   = 0x0001F4D8
                                          , teLength   = 0x00000036
                                          , teData     = FTRaw ""
                                          }
                             , TableEntry { teTag = toTableIdentifier "hhea"
                                          , teChecksum = 0x0ABA0AAE
                                          , teOffset   = 0x00020948
                                          , teLength   = 0x00000024
                                          , teData     = FTRaw ""
                                          }
                             , TableEntry { teTag = toTableIdentifier "hmtx"
                                          , teChecksum = 0xAE728F97
                                          , teOffset   = 0x0001F510
                                          , teLength   = 0x00001438
                                          , teData     = FTRaw ""
                                          }
                             , TableEntry { teTag = toTableIdentifier "loca"
                                          , teChecksum = 0x8077FFBB
                                          , teOffset   = 0x0001EAB8
                                          , teLength   = 0x00000A1E
                                          , teData     = FTRaw ""
                                          }
                             , TableEntry { teTag = toTableIdentifier "maxp"
                                          , teChecksum = 0x073E0309
                                          , teOffset   = 0x0001EA98
                                          , teLength   = 0x00000020
                                          , teData     = FTRaw ""
                                          }
                             , TableEntry { teTag = toTableIdentifier "name"
                                          , teChecksum = 0x362161D6
                                          , teOffset   = 0x000218C4
                                          , teLength   = 0x00000292
                                          , teData     = FTRaw ""
                                          }
                             , TableEntry { teTag = toTableIdentifier "post"
                                          , teChecksum = 0xFF6D0064
                                          , teOffset   = 0x00021B58
                                          , teLength   = 0x00000020
                                          , teData     = FTRaw ""
                                          }
                             , TableEntry { teTag = toTableIdentifier "prep"
                                          , teChecksum = 0xA266FAC9
                                          , teOffset   = 0x00021724
                                          , teLength   = 0x00000149
                                          , teData     = FTRaw ""
                                          }
                             ]
      }
    )
  ]

calcChecksumExamples :: [(BS.ByteString, Word32)]
calcChecksumExamples =
  [ (""        , 0)
  , ("a"       , 0x61000000)
  , ("ab"      , 0x61620000)
  , ("abc"     , 0x61626300)
  , ("abcd"    , 0x61626364)
  , ("abcde"   , 0x61626364 + 0x65000000)
  , ("abcdef"  , 0x61626364 + 0x65660000)
  , ("abcdefg" , 0x61626364 + 0x65666700)
  , ("\x00\x00\x00\x01\x00\x00\x00\x01", 2)
  , ("\x00\x00\x00\x01\x00\x00\x00\x01\x00", 2)
  , ("\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01", 3)
  , ("\xFF\xFF\xFF\xFF\x00\x00\x00\x02", 1)
  , ("abcdefgh", 0x61626364 + 0x65666768)
  ]

resetData :: FontDirectory -> FontDirectory
resetData fd = fd { fdTableDirectory = loadContent "" <$> fdTableDirectory fd }

spec :: Spec
spec = do
  describe "fontDirectoryP" $ do
    forM_ fontExamples $ \(filePath, fontDirectory) ->
      it ("decodes file " ++ show filePath) $ do
        fontFile <- BS.readFile ("test/Font/TrueType/Parser/" ++ filePath)
        resetData <$> ttfParse fontFile `shouldBe` Right fontDirectory

  describe "calcChecksum" $ do
    forM_ calcChecksumExamples $ \(source, expected) ->
      it ("calculates checksum for " ++ show source) $ do
        calcChecksum source `shouldBe` expected

  describe "calcTableChecksum" $ do
    forM_ fontExamples $ \(filePath, _) ->
      it ("calculate checksum for " ++ show filePath) $ do
        fontFile <- BS.readFile ("test/Font/TrueType/Parser/" ++ filePath)
        case ttfParse fontFile of
          Left _ -> return ()
          Right fd ->
            forM_ (fdTableDirectory fd)
              $ \entry ->
                  (entry, calcTableChecksum entry)
                    `shouldBe` (entry, teChecksum entry)
