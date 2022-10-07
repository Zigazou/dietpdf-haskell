{-# LANGUAGE OverloadedStrings #-}

module Pdf.Object.ContainerSpec
  ( spec
  ) where

import           Control.Monad                  ( forM_ )
import qualified Data.ByteString               as BS
import           Data.Map.Strict                ( fromList
                                                , empty
                                                )
import           Pdf.Object.Object              ( Dictionary
                                                , PDFObject
                                                  ( PDFArray
                                                  , PDFDictionary
                                                  , PDFIndirectObject
                                                  , PDFName
                                                  , PDFNull
                                                  , PDFNumber
                                                  , PDFString
                                                  )
                                                )
import           Pdf.Object.State               ( updateE )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )

import           Pdf.Object.Container           ( Filter(Filter)
                                                , deepMap
                                                , insertMaybes
                                                , setFilters
                                                )
import           Util.Errors                    ( UnifiedError )

deepMapExamples
  :: [(PDFObject, PDFObject -> Either UnifiedError PDFObject, PDFObject)]
deepMapExamples =
  [ ( PDFArray [PDFDictionary empty, PDFNumber 3.0, PDFName "ABCD"]
    , addOneToAnyNumber
    , PDFArray [PDFDictionary empty, PDFNumber 4.0, PDFName "ABCD"]
    )
  , ( PDFDictionary (fromList [("X", PDFNumber 1.0), ("Y", PDFString "abcd")])
    , addOneToAnyNumber
    , PDFDictionary (fromList [("X", PDFNumber 2.0), ("Y", PDFString "abcd")])
    )
  , ( PDFIndirectObject
      2
      0
      (PDFDictionary (fromList [("X", PDFNumber 1.0), ("Y", PDFString "abcd")]))
    , addOneToAnyNumber
    , PDFIndirectObject
      2
      0
      (PDFDictionary (fromList [("X", PDFNumber 2.0), ("Y", PDFString "abcd")]))
    )
  , ( PDFArray
      [ PDFDictionary (fromList [("X", PDFNumber 1.0)])
      , PDFNumber 3.0
      , PDFName "ABCD"
      ]
    , addOneToAnyNumber
    , PDFArray
      [ PDFDictionary (fromList [("X", PDFNumber 2.0)])
      , PDFNumber 4.0
      , PDFName "ABCD"
      ]
    )
  ]
 where
  addOneToAnyNumber :: PDFObject -> Either UnifiedError PDFObject
  addOneToAnyNumber (PDFNumber x) = return (PDFNumber (x + 1.0))
  addOneToAnyNumber object        = return object

insertMaybesExamples :: [([(BS.ByteString, Maybe PDFObject)], Dictionary)]
insertMaybesExamples =
  [ ( [("a", Just PDFNull), ("b", Nothing), ("c", Just (PDFNumber 2.0))]
    , fromList [("a", PDFNull), ("c", PDFNumber 2.0)]
    )
  , ( [("a", Just PDFNull), ("b", Nothing), ("a", Just (PDFNumber 2.0))]
    , fromList [("a", PDFNumber 2.0)]
    )
  ]

updateFiltersExamples :: [([Filter], PDFObject, PDFObject)]
updateFiltersExamples =
  [ ( [Filter (PDFName "FlateDecode") PDFNull]
    , PDFIndirectObject 1 0 (PDFDictionary empty)
    , PDFIndirectObject
      1
      0
      (PDFDictionary (fromList [("Filter", PDFName "FlateDecode")]))
    )
  , ( [Filter (PDFName "FlateDecode") (PDFName "Foo")]
    , PDFIndirectObject 1 0 (PDFDictionary empty)
    , PDFIndirectObject
      1
      0
      (PDFDictionary
        (fromList
          [("Filter", PDFName "FlateDecode"), ("DecodeParms", PDFName "Foo")]
        )
      )
    )
  , ( [ Filter (PDFName "RLEDecode")   PDFNull
      , Filter (PDFName "FlateDecode") (PDFName "foo")
      ]
    , PDFIndirectObject 1 0 (PDFDictionary empty)
    , PDFIndirectObject
      1
      0
      (PDFDictionary
        (fromList
          [ ("Filter", PDFArray [PDFName "RLEDecode", PDFName "FlateDecode"])
          , ("DecodeParms", PDFArray [PDFNull, PDFName "Foo"])
          ]
        )
      )
    )
  ]

spec :: Spec
spec = do
  describe "deepMap" $ forM_ deepMapExamples $ \(example, fn, expected) ->
    it ("should give right result for " ++ show example)
      $          updateE example (deepMap fn)
      `shouldBe` Right expected

  describe "insertMaybes"
    $ forM_ insertMaybesExamples
    $ \(example, expected) ->
        it ("should give right result for " ++ show example)
          $          insertMaybes empty example
          `shouldBe` expected

  describe "update+setFilters"
    $ forM_ updateFiltersExamples
    $ \(filters, example, expected) ->
        it ("should give right result for " ++ show filters)
          $          updateE example (setFilters filters)
          `shouldBe` Right expected
