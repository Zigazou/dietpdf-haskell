{-# LANGUAGE OverloadedStrings #-}

module Pdf.Object.ContainerSpec
  ( spec
  ) where

import           Control.Monad                  ( forM_ )
import qualified Data.ByteString               as BS
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFIndirectObject
                                                  , PDFName
                                                  , PDFNull
                                                  , PDFNumber
                                                  , PDFString
                                                  )
                                                , mkPDFArray
                                                , mkPDFDictionary
                                                , mkEmptyPDFDictionary
                                                )
import           Util.Dictionary                ( Dictionary
                                                , mkDictionary
                                                , mkEmptyDictionary
                                                )
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
import           Util.UnifiedError              ( FallibleT
                                                )
import           Control.Monad.Trans.Except     ( runExceptT )

deepMapExamples
  :: [(PDFObject, PDFObject -> FallibleT IO PDFObject, PDFObject)]
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
  addOneToAnyNumber :: PDFObject -> FallibleT IO PDFObject
  addOneToAnyNumber (PDFNumber x) = return (PDFNumber (x + 1.0))
  addOneToAnyNumber object        = return object

insertMaybesExamples
  :: [([(BS.ByteString, Maybe PDFObject)], Dictionary PDFObject)]
insertMaybesExamples =
  [ ( [("a", Just PDFNull), ("b", Nothing), ("c", Just (PDFNumber 2.0))]
    , mkDictionary [("a", PDFNull), ("c", PDFNumber 2.0)]
    )
  , ( [("a", Just PDFNull), ("b", Nothing), ("a", Just (PDFNumber 2.0))]
    , mkDictionary [("a", PDFNumber 2.0)]
    )
  ]

filtersExamples :: [(FilterList, PDFObject, PDFObject)]
filtersExamples =
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
    it ("should give right result for " ++ show example) $ do
      result <- runExceptT $ deepMap fn example
      result `shouldBe` Right expected

  describe "insertMaybes"
    $ forM_ insertMaybesExamples
    $ \(example, expected) ->
        it ("should give right result for " ++ show example)
          $          insertMaybes mkEmptyDictionary example
          `shouldBe` expected

  describe "setFilters"
    $ forM_ filtersExamples
    $ \(filters, example, expected) ->
        it ("should give right result for " ++ show filters)
          $ do
            result <- runExceptT $ setFilters filters example
            result `shouldBe` Right expected
