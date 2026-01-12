module Font.TrueType.Parser.FontSpec
  ( spec
  ) where

import Control.Monad (forM_)

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.UnifiedError (UnifiedError (UnableToOpenFile))
import Data.Word (Word32)

import Font.TrueType.FontDirectory
  ( FontDirectory (FontDirectory, fdOffsetSubtable, fdTableDirectory)
  , fromFontDirectory
  )
import Font.TrueType.FontTable (FontTable (FTRaw))
import Font.TrueType.OffsetSubtable
  ( OffsetSubtable (OffsetSubtable, osEntrySelector, osNumTables, osRangeShift, osScalerType, osSearchRange)
  )
import Font.TrueType.Parser.Font (ttfParse)
import Font.TrueType.ScalerType (ScalerType (FontTrueType00010000))
import Font.TrueType.TableDirectory (mkTableDirectory)
import Font.TrueType.TableEntry
  ( TableEntry (TableEntry, teChecksum, teData, teLength, teOffset, teTag)
  , calcChecksum
  , loadContent
  )
import Font.TrueType.TableIdentifier (toTableIdentifier)

import Test.Hspec (Spec, describe, it, shouldBe)

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
      , fdTableDirectory = mkTableDirectory
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

calcChecksumExamples :: [(ByteString, Word32)]
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

fromFontDirectoryExamples :: [(FontDirectory, ByteString)]
fromFontDirectoryExamples =
  [ ( FontDirectory
      { fdOffsetSubtable = OffsetSubtable { osScalerType = FontTrueType00010000
                                          , osNumTables     = 1
                                          , osSearchRange   = 0x0000
                                          , osEntrySelector = 0x0000
                                          , osRangeShift    = 0x0000
                                          }
      , fdTableDirectory = mkTableDirectory
                             [ TableEntry { teTag = toTableIdentifier "GDEF"
                                          , teChecksum = calcChecksum "ABCDEFGH"
                                          , teOffset   = 0
                                          , teLength   = fromIntegral $ BS.length "ABCDEFGH"
                                          , teData     = FTRaw "ABCDEFGH"
                                          }
                             ]
      }
    , "\x00\x01\x00\x00\x00\x01\x00\x10\x00\x00\x00\x00\
      \GDEF\x86\x88\x8A\x8C\x00\x00\x00\x1c\x00\x00\x00\x08\&ABCDEFGH"
    )
  , ( FontDirectory
      { fdOffsetSubtable = OffsetSubtable { osScalerType = FontTrueType00010000
                                          , osNumTables     = 2
                                          , osSearchRange   = 0x0000
                                          , osEntrySelector = 0x0000
                                          , osRangeShift    = 0x0000
                                          }
      , fdTableDirectory = mkTableDirectory
                            [ TableEntry  { teTag = toTableIdentifier "GDEF"
                                          , teChecksum = calcChecksum "ABCDEFGH"
                                          , teOffset   = 0
                                          , teLength   = fromIntegral $ BS.length "ABCDEFGH"
                                          , teData     = FTRaw "ABCDEFGH"
                                          }
                            , TableEntry  { teTag = toTableIdentifier "glyf"
                                          , teChecksum = calcChecksum "1234"
                                          , teOffset   = 0
                                          , teLength   = fromIntegral $ BS.length "1234"
                                          , teData     = FTRaw "1234"
                                          }
                            ]
      }
    , "\x00\x01\x00\x00\x00\x02\x00\x20\x00\x01\x00\x00\
      \glyf\x31\x32\x33\x34\x00\x00\x00\x2c\x00\x00\x00\x04\
      \GDEF\x86\x88\x8A\x8C\x00\x00\x00\x30\x00\x00\x00\x08\
      \1234\
      \ABCDEFGH"
    )
  ]

reversibilityExamples :: [FilePath]
reversibilityExamples =
  [ "Roboto/Roboto-Regular.ttf"
  , "Roboto/Roboto-BlackItalic.ttf"
  , "Roboto/Roboto-Black.ttf"
  , "Roboto/Roboto-BoldItalic.ttf"
  , "Roboto/Roboto-Bold.ttf"
  , "Roboto/Roboto-Italic.ttf"
  , "Roboto/Roboto-LightItalic.ttf"
  , "Roboto/Roboto-Light.ttf"
  , "Roboto/Roboto-MediumItalic.ttf"
  , "Roboto/Roboto-Medium.ttf"
  , "Roboto/Roboto-Regular.ttf"
  , "Roboto/Roboto-ThinItalic.ttf"
  , "Roboto/Roboto-Thin.ttf"
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

  describe "fromFontDirectory" $ do
    forM_ fromFontDirectoryExamples $ \(fontDirectory, expected) ->
      it ("serializes font directory " ++ show fontDirectory) $ do
        fromFontDirectory fontDirectory `shouldBe` expected

  describe "reversibility" $ do
    forM_ reversibilityExamples $ \filePath ->
      it ("recreates the original file" ++ show filePath) $ do
        originalFile <- BS.readFile ("test/Font/TrueType/Parser/" ++ filePath)
        let binary1 = fromFontDirectory <$> ttfParse originalFile
            binary2 = case binary1 of
              Left _         -> Left UnableToOpenFile
              Right binary1' -> fromFontDirectory <$> ttfParse binary1'

        binary1 `shouldBe` binary2
