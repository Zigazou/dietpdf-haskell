{-# LANGUAGE OverloadedStrings #-}

module Pdf.Object.ContainerSpec
  ( spec
  ) where

import           Control.Monad                  ( forM_ )
import qualified Data.ByteString               as BS
import           Pdf.Object.Object              ( Dictionary
                                                , PDFObject
                                                  ( PDFIndirectObject
                                                  , PDFName
                                                  , PDFNull
                                                  , PDFNumber
                                                  , PDFString
                                                  )
                                                , mkPDFArray
                                                , mkPDFDictionary
                                                , mkEmptyPDFDictionary
                                                , mkDictionary
                                                , mkEmptyDictionary
                                                )
import           Pdf.Object.State               ( updateE )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )

import           Pdf.Object.Container           ( Filter(Filter)
                                                , FilterList
                                                , mkFilterList
                                                , deepMap
                                                , insertMaybes
                                                , setFilters
                                                )
import           Util.Errors                    ( UnifiedError )

deepMapExamples
  :: [(PDFObject, PDFObject -> Either UnifiedError PDFObject, PDFObject)]
deepMapExamples =
  [ ( mkPDFArray [mkEmptyPDFDictionary, PDFNumber 3.0, PDFName "ABCD"]
    , addOneToAnyNumber
    , mkPDFArray [mkEmptyPDFDictionary, PDFNumber 4.0, PDFName "ABCD"]
    )
  , ( mkPDFDictionary [("X", PDFNumber 1.0), ("Y", PDFString "abcd")]
    , addOneToAnyNumber
    , mkPDFDictionary [("X", PDFNumber 2.0), ("Y", PDFString "abcd")]
    )
  , ( PDFIndirectObject
      2
      0
      (mkPDFDictionary [("X", PDFNumber 1.0), ("Y", PDFString "abcd")])
    , addOneToAnyNumber
    , PDFIndirectObject
      2
      0
      (mkPDFDictionary [("X", PDFNumber 2.0), ("Y", PDFString "abcd")])
    )
  , ( mkPDFArray
      [mkPDFDictionary [("X", PDFNumber 1.0)], PDFNumber 3.0, PDFName "ABCD"]
    , addOneToAnyNumber
    , mkPDFArray
      [mkPDFDictionary [("X", PDFNumber 2.0)], PDFNumber 4.0, PDFName "ABCD"]
    )
  ]
 where
  addOneToAnyNumber :: PDFObject -> Either UnifiedError PDFObject
  addOneToAnyNumber (PDFNumber x) = return (PDFNumber (x + 1.0))
  addOneToAnyNumber object        = return object

insertMaybesExamples :: [([(BS.ByteString, Maybe PDFObject)], Dictionary)]
insertMaybesExamples =
  [ ( [("a", Just PDFNull), ("b", Nothing), ("c", Just (PDFNumber 2.0))]
    , mkDictionary [("a", PDFNull), ("c", PDFNumber 2.0)]
    )
  , ( [("a", Just PDFNull), ("b", Nothing), ("a", Just (PDFNumber 2.0))]
    , mkDictionary [("a", PDFNumber 2.0)]
    )
  ]

updateFiltersExamples :: [(FilterList, PDFObject, PDFObject)]
updateFiltersExamples =
  [ ( mkFilterList [Filter (PDFName "FlateDecode") PDFNull]
    , PDFIndirectObject 1 0 mkEmptyPDFDictionary
    , PDFIndirectObject 1
                        0
                        (mkPDFDictionary [("Filter", PDFName "FlateDecode")])
    )
  , ( mkFilterList [Filter (PDFName "FlateDecode") (PDFName "Foo")]
    , PDFIndirectObject 1 0 mkEmptyPDFDictionary
    , PDFIndirectObject
      1
      0
      (mkPDFDictionary
        [("Filter", PDFName "FlateDecode"), ("DecodeParms", PDFName "Foo")]
      )
    )
  , ( mkFilterList
      [ Filter (PDFName "RLEDecode")   PDFNull
      , Filter (PDFName "FlateDecode") (PDFName "foo")
      ]
    , PDFIndirectObject 1 0 mkEmptyPDFDictionary
    , PDFIndirectObject
      1
      0
      (mkPDFDictionary
        [ ("Filter", mkPDFArray [PDFName "RLEDecode", PDFName "FlateDecode"])
        , ("DecodeParms", mkPDFArray [PDFNull, PDFName "Foo"])
        ]
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
          $          insertMaybes mkEmptyDictionary example
          `shouldBe` expected

  describe "update+setFilters"
    $ forM_ updateFiltersExamples
    $ \(filters, example, expected) ->
        it ("should give right result for " ++ show filters)
          $          updateE example (setFilters filters)
          `shouldBe` Right expected
