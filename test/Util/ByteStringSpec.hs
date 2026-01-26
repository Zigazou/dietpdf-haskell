module Util.ByteStringSpec
  ( spec
  ) where

import Control.Monad (forM_)

import Data.ByteString (ByteString)
import Data.Map (Map, fromList)
import Data.TranslationTable (getTranslationTable)

import Test.Hspec (Spec, describe, it, shouldBe)

import Util.ByteString
  ( containsOnlyGray
  , convertToGray
  , groupComponents
  , optimizeParity
  , separateComponents
  , splitRaw
  , toNameBase
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
