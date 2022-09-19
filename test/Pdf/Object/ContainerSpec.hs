{-# LANGUAGE OverloadedStrings #-}
module Pdf.Object.ContainerSpec
  ( spec
  ) where

import           Control.Monad                  ( forM_ )
import           Data.HashMap.Strict            ( fromList )
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFNumber
                                                  , PDFName
                                                  , PDFDictionary
                                                  , PDFArray
                                                  , PDFString
                                                  , PDFIndirectObject
                                                  )
                                                )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )

import           Pdf.Object.Container           ( deepMap )

deepMapExamples :: [(PDFObject, PDFObject -> PDFObject, PDFObject)]
deepMapExamples =
  [ ( PDFArray [PDFDictionary (fromList []), PDFNumber 3.0, PDFName "ABCD"]
    , addOneToAnyNumber
    , PDFArray [PDFDictionary (fromList []), PDFNumber 4.0, PDFName "ABCD"]
    )
  , ( PDFDictionary (fromList [("X", PDFNumber 1.0), ("Y", PDFString "abcd")])
    , addOneToAnyNumber
    , PDFDictionary (fromList [("X", PDFNumber 2.0), ("Y", PDFString "abcd")])
    )
  , ( PDFIndirectObject
      2
      0
      (PDFDictionary (fromList [("X", PDFNumber 1.0), ("Y", PDFString "abcd")]))
      Nothing
    , addOneToAnyNumber
    , PDFIndirectObject
      2
      0
      (PDFDictionary (fromList [("X", PDFNumber 2.0), ("Y", PDFString "abcd")]))
      Nothing
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
  addOneToAnyNumber :: PDFObject -> PDFObject
  addOneToAnyNumber (PDFNumber x) = PDFNumber (x + 1.0)
  addOneToAnyNumber object        = object

spec :: Spec
spec = do
  describe "deepMap" $ forM_ deepMapExamples $ \(example, fn, expected) ->
    it ("should give right result for " ++ show example)
      $          deepMap fn example
      `shouldBe` expected
