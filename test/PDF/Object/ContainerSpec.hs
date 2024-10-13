{-# LANGUAGE OverloadedStrings #-}

module PDF.Object.ContainerSpec
  ( spec
  ) where

import Control.Monad (forM_)

import Data.PDF.Filter (Filter (Filter))
import Data.PDF.FilterList (FilterList, mkFilterList)
import Data.PDF.PDFWork (PDFWork, evalPDFWorkT)

import PDF.Object.Container (setFilters)
import PDF.Object.Object
    ( PDFObject (PDFIndirectObject, PDFName, PDFNull, PDFNumber, PDFString)
    , mkEmptyPDFDictionary
    , mkPDFArray
    , mkPDFDictionary
    )
import PDF.Processing.PDFWork (deepMapP)

import Test.Hspec (Spec, describe, it, shouldBe)

deepMapExamples
  :: [(PDFObject, PDFObject -> PDFWork IO PDFObject, PDFObject)]
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
  addOneToAnyNumber :: PDFObject -> PDFWork IO PDFObject
  addOneToAnyNumber (PDFNumber x) = return (PDFNumber (x + 1.0))
  addOneToAnyNumber object        = return object

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
      result <- evalPDFWorkT $ deepMapP fn example
      result `shouldBe` Right expected

  describe "setFilters"
    $ forM_ filtersExamples
    $ \(filters, example, expected) ->
        it ("should give right result for " ++ show filters)
          $ do
            result <- evalPDFWorkT $ setFilters filters example
            result `shouldBe` Right expected
