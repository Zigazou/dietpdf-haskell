{-# LANGUAGE OverloadedStrings #-}

module PDF.Object.ContainerSpec
  ( spec
  ) where

import Control.Monad (forM_)

import Data.PDF.Filter (Filter (Filter))
import Data.PDF.FilterList (FilterList, mkFilterList)
import Data.PDF.PDFWork (evalPDFWorkT)

import PDF.Object.Container (setFilters)
import PDF.Object.Object
    ( PDFObject (PDFIndirectObject, PDFName, PDFNull)
    , mkEmptyPDFDictionary
    , mkPDFArray
    , mkPDFDictionary
    )

import Test.Hspec (Spec, describe, it, shouldBe)

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
  describe "setFilters"
    $ forM_ filtersExamples
    $ \(filters, example, expected) ->
        it ("should give right result for " ++ show filters)
          $ do
            result <- evalPDFWorkT $ setFilters filters example
            result `shouldBe` Right expected
