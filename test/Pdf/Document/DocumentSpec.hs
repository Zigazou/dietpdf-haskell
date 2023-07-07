{-# LANGUAGE OverloadedStrings #-}
module Pdf.Document.DocumentSpec
  ( spec
  ) where

import           Control.Monad                  ( forM_ )
import           Pdf.Document.Document          ( deepFind
                                                , fromList
                                                )
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFIndirectObject
                                                  , PDFName
                                                  , PDFNull
                                                  , PDFNumber
                                                  , PDFTrailer
                                                  , PDFVersion
                                                  )
                                                , mkPDFArray
                                                , mkPDFDictionary
                                                )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )

createExamples :: [([PDFObject], Int)]
createExamples =
  [ ([]                          , 0)
  , ([PDFName "a"]               , 1)
  , ([PDFName "a", PDFNumber 1.0], 2)
  , ([PDFName "a", PDFName "b"]  , 2)
  , ([PDFName "a", PDFName "a"]  , 1)
  , ([PDFName "a", PDFName "a"]  , 1)
  , ([PDFVersion "1.2", PDFVersion "1.5"], 1)
  , ([PDFIndirectObject 1 0 PDFNull, PDFIndirectObject 1 0 (PDFName "a")], 1)
  ]

deepFindExamples :: [([PDFObject], PDFObject -> Bool, [PDFObject])]
deepFindExamples =
  [ ([]                        , predicate, [])
  , ([PDFName "a"]             , predicate, [PDFName "a"])
  , ([PDFName "b", PDFName "a"], predicate, [PDFName "b", PDFName "a"])
  , ([PDFVersion "1.2", PDFVersion "1.5"], predicate, [])
  , ( [PDFIndirectObject 2 0 PDFNull, PDFIndirectObject 1 0 (PDFName "a")]
    , predicate
    , [PDFName "a"]
    )
  , ([PDFIndirectObject 1 0 (PDFName "a")], predicate, [PDFName "a"])
  , ( [PDFIndirectObject 1 0 PDFNull, PDFIndirectObject 1 0 (PDFName "a")]
    , predicate
    , [PDFName "a"]
    )
  , ( [PDFIndirectObject 1 0 (PDFName "a"), PDFIndirectObject 1 0 PDFNull]
    , predicate
    , []
    )
  , ( [PDFIndirectObject 1 0 (mkPDFArray [PDFNull, PDFName "a"])]
    , predicate
    , [PDFName "a"]
    )
  , ( [ PDFIndirectObject
          1
          0
          (mkPDFArray [PDFNull, PDFName "a", mkPDFArray [PDFName "b"]])
      ]
    , predicate
    , [PDFName "a", PDFName "b"]
    )
  , ( [PDFTrailer (mkPDFDictionary [("a", PDFName "a"), ("b", PDFName "b")])]
    , predicate
    , [PDFName "a", PDFName "b"]
    )
  ]
 where
  predicate :: PDFObject -> Bool
  predicate (PDFName _)     = True
  predicate _anyOtherObject = False

spec :: Spec
spec = do
  describe "fromList" $ do
    forM_ createExamples $ \(example, expected) ->
      it ("should have " ++ show expected ++ " items from " ++ show example)
        $          (length . fromList) example
        `shouldBe` expected

  describe "deepFind" $ do
    forM_ deepFindExamples $ \(example, predicate, expected) ->
      it ("should work with " ++ show example)
        $          (deepFind predicate . fromList) example
        `shouldBe` fromList expected
