module Util.ByteStringSpec
  ( spec
  ) where

import Control.Monad (forM_)

import Data.ByteString (ByteString)
import Data.Map (Map, fromList)
import Data.TranslationTable (getTranslationTable)
import Data.Word (Word16, Word8)

import Test.Hspec (Spec, describe, it, shouldBe)

import Util.ByteString
  ( containsOnlyGray
  , convertToGray
  , groupComponents
  , optimizeParity
  , pack16BitBE
  , packBits
  , separateComponents
  , splitRaw
  , toNameBase
  , unpack16BitBE
  , unpackBits
  )

splitRawExamples :: [(ByteString, Int, [ByteString])]
splitRawExamples =
  [ ("ABCD", 2, ["AB", "CD"])
  , ("ABCD", 1, ["A", "B", "C", "D"])
  , ("ABCD", 3, ["ABC", "D"])
  , (""    , 3, [])
  ]

separateComponentsExamples :: [(ByteString, Int, [ByteString])]
separateComponentsExamples =
  [ ("ABCD", 2, ["AC", "BD"])
  , ("ABCD", 1, ["ABCD"])
  , ("ABCD", 3, ["AD", "B", "C"])
  , (""    , 1, [""])
  ]

groupComponentsExamples :: [([ByteString], ByteString)]
groupComponentsExamples =
  [ (["AC", "BD"]    , "ABCD")
  , (["ABCD"]        , "ABCD")
  , (["AD", "B", "C"], "ABCD")
  , ([""]            , "")
  ]

toNameBaseExamples :: [(Int, ByteString)]
toNameBaseExamples =
  [ (0, "0")
  , (1, "1")
  , (2, "2")
  , (3, "3")
  , (10, "a")
  , (11, "b")
  , (61, "Z")
  , (62, "10")
  , (72, "1a")
  , (123, "1Z")
  , (124, "20")
  , (3843, "ZZ")
  , (3844, "100")
  ]

renameStringsExamples :: [([ByteString], Map ByteString ByteString)]
renameStringsExamples =
  [ ([], mempty)
  , (["A", "B", "C"], fromList [("A", "0"), ("B", "1"), ("C", "2")])
  , ( [ "A10", "A11", "A12", "A13", "A14"
      , "A0", "A1", "A2", "A3", "A4"
      , "A00", "A01", "A02", "A03", "A04"
      , "A0", "A1", "A2", "A3", "A4"
      ]
    , fromList
      [ ("A0","0"), ("A1","1"), ("A2","2"), ("A3","3"), ("A4","4")
      , ("A00","5"), ("A01","6"), ("A02","7"), ("A03","8"), ("A04","9")
      , ("A10","a"), ("A11","b"), ("A12","c"), ("A13","d"), ("A14","e")
      ]
    )
  ]

containsOnlyGrayExamples :: [(ByteString, Bool)]
containsOnlyGrayExamples =
  [ ("AAAAAA", True)
  , ("AAABBBCCC", True)
  , ("ABABAB", True)
  , ("ATXBUZ", False)
  , ("",       True)
  , ("AAAAA" , False)
  ]

convertToGrayExamples :: [(ByteString, ByteString)]
convertToGrayExamples =
  [ ("AAAAAA", "AA")
  , ("AAABBBCCC", "ABC")
  , ("ABABAB", "AB")
  , ("",       "")
  , ("AAAAA" , "AAAAA")
  ]

optimizeParityExamples :: [(ByteString, ByteString)]
optimizeParityExamples =
  [ ("\xFF\xFF\xFF", "\xFF\xFF\xFF")
  , ("\x00\x00\x00", "\x00\x00\x00")
  , ("\xFE\xFE\xFF", "\xFE\xFE\xFE")
  , ("\xFF\xFE\xFF", "\xFF\xFF\xFF")
  , ("\xFE\xFF\xFE", "\xFE\xFE\xFE")
  , ("\x01\x02\x03", "\x01\x03\x03")
  , ("", "")                            -- Empty ByteString
  , ("\xFF\xFF", "\xFF\xFF")            -- Incomplete triplet (less than 3 bytes): no change
  , ("\xFF\xFF\xFF\xFE\xFE\xFE", "\xFF\xFF\xFF\xFE\xFE\xFE")  -- Two triplets
  , ("012345678", "002355668")  -- Longer ByteString with mixed values
  ]

unpackBitsExamples :: [(Int, ByteString, [Word8])]
unpackBitsExamples =
  [ (2, "\x88", [2, 0, 2, 0])                 -- 10 00 10 00
  , (2, "\xE4", [3, 2, 1, 0])                 -- 11 10 01 00
  , (2, "\xE4\x1B", [3, 2, 1, 0, 0, 1, 2, 3]) -- 11 10 01 00 00 01 10 11
  , (4, "\xA4", [10, 4])                      -- 1010 0100
  , (4, "\xA4\x7D", [10, 4, 7, 13])           -- 1010 0100 0111 1101
  , (8, "\xFF\x00\x7F", [255, 0, 127])        -- FFFF FFFF 0000 0000 0111 1111
  , (1, "\x05", [0, 0, 0, 0, 0, 1, 0, 1])     -- 0 0 0 0 0 1 0 1
  , (2, "", [])                               -- Empty ByteString
  , (4, "", [])                               -- Empty ByteString
  , (8, "", [])                               -- Empty ByteString
  ]

packBitsExamples :: [(Int, [Word8], ByteString)]
packBitsExamples =
  [ (2, [3, 2, 1, 0]            , "\xE4"        ) -- 11 10 01 00
  , (2, [2, 0, 2, 0]            , "\x88"        ) -- 10 00 10 00
  , (2, [3, 2, 1, 0, 0, 1, 2, 3], "\xE4\x1B"    ) -- 11 10 01 00 00 01 10 11
  , (4, [10, 4]                 , "\xA4"        ) -- 1010 0100
  , (4, [10, 4, 7, 13]          , "\xA4\x7D"    ) -- 1010 0100 0111 1101
  , (8, [255, 0, 127]           , "\xFF\x00\x7F") -- FFFF FFFF 0000 0000 0111 1111
  , (1, [0, 0, 0, 0, 0, 1, 0, 1], "\x05"        ) -- 0 0 0 0 0 1 0 1
  , (2, []                      , ""            ) -- Empty ByteString
  , (4, []                      , ""            ) -- Empty ByteString
  , (8, []                      , ""            ) -- Empty ByteString
  , (1, [1, 0, 1, 0, 1]         , "\xA8"        ) -- 1 0 1 0 1 0 0 0
  , (2, [2, 2, 1]               , "\xA4"        ) -- 10 10 01 00
  , (4, [15]                    , "\xF0"        ) -- 1111 0000
  ]

unpack16BitBEExamples :: [(ByteString, [Word16])]
unpack16BitBEExamples =
  [ ("\x12\x34\x56\x78", [0x1234, 0x5678])
  , ("\xFF\xFF\x00\x00", [0xFFFF, 0x0000])
  , ("\x00\x01\x00\x02", [0x0001, 0x0002])
  , ("\x00\x01", [0x0001])
  , ("", [])
  , ("\xAB", [0xAB00])
  ]

pack16BitBEExamples :: [([Word16], ByteString)]
pack16BitBEExamples =
  [ ([0x1234, 0x5678], "\x12\x34\x56\x78")
  , ([0xFFFF, 0x0000], "\xFF\xFF\x00\x00")
  , ([0x0001, 0x0002], "\x00\x01\x00\x02")
  , ([0x0001], "\x00\x01")
  , ([], "")
  , ([0xAB00], "\xAB\x00")
  ]

spec :: Spec
spec = do
  describe "splitRaw" $ forM_ splitRawExamples $ \(example, width, expected) ->
    it
        (  "should split ByteString in strings of "
        ++ show width
        ++ " bytes "
        ++ show example
        )
      $          splitRaw width example
      `shouldBe` expected

  describe "separateComponents"
    $ forM_ separateComponentsExamples
    $ \(example, components, expected) ->
        it
            (  "should separate components strings of "
            ++ show components
            ++ " components "
            ++ show example
            )
          $          separateComponents components example
          `shouldBe` expected

  describe "groupComponents"
    $ forM_ groupComponentsExamples
    $ \(example, expected) ->
        it ("should group components strings " ++ show example)
          $          groupComponents example
          `shouldBe` expected

  describe "toNameBase"
    $ forM_ toNameBaseExamples
    $ \(example, expected) ->
        it ("should convert " ++ show example ++ " to " ++ show expected)
          $          toNameBase example
          `shouldBe` expected

  describe "getTranslationTable"
    $ forM_ renameStringsExamples
    $ \(example, expected) ->
        it ("should rename strings " ++ show example)
          $          getTranslationTable (\_ a -> toNameBase a) example
          `shouldBe` expected

  describe "containsOnlyGray"
    $ forM_ containsOnlyGrayExamples
    $ \(example, expected) ->
        it ("should detect gray-only RGB ByteString " ++ show example)
          $          containsOnlyGray example
          `shouldBe` expected

  describe "convertToGray"
    $ forM_ convertToGrayExamples
    $ \(example, expected) ->
        it ("should convert gray-only RGB ByteString " ++ show example)
          $          convertToGray example
          `shouldBe` expected

  describe "optimizeParity"
    $ forM_ optimizeParityExamples
    $ \(example, expected) ->
        it ("should optimize parity of RGB triplets " ++ show example)
          $          optimizeParity example
          `shouldBe` expected

  describe "unpackBits"
    $ forM_ unpackBitsExamples
    $ \(bitWidth, example, expected) ->
        it
            (  "should unpack bits of width "
            ++ show bitWidth
            ++ " from ByteString "
            ++ show example
            )
          $          unpackBits bitWidth example
          `shouldBe` expected

  describe "packBits"
    $ forM_ packBitsExamples
    $ \(bitWidth, example, expected) ->
        it
            (  "should pack bits of width "
            ++ show bitWidth
            ++ " from list "
            ++ show example
            )
          $          packBits bitWidth example
          `shouldBe` expected

  describe "unpack16BitBE"
    $ forM_ unpack16BitBEExamples
    $ \(example, expected) ->
        it ("should unpack 16-bit BE from ByteString " ++ show example)
          $          unpack16BitBE example
          `shouldBe` expected

  describe "pack16BitBE"
    $ forM_ pack16BitBEExamples
    $ \(example, expected) ->
        it ("should pack 16-bit BE from list " ++ show example)
          $          pack16BitBE example
          `shouldBe` expected
