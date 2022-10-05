{-# LANGUAGE OverloadedStrings #-}
module Pdf.Document.DocumentSpec
  ( spec
  ) where

import           Control.Monad                  ( forM_ )
import qualified Data.HashMap.Strict           as HM
import           Pdf.Document.Document          ( clean
                                                , deepFind
                                                , fromList
                                                )
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFArray
                                                  , PDFDictionary
                                                  , PDFIndirectObject
                                                  , PDFName
                                                  , PDFNull
                                                  , PDFNumber
                                                  , PDFTrailer
                                                  , PDFVersion
                                                  )
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

cleanExamples :: [([PDFObject], [PDFObject])]
cleanExamples =
  [ ([]                          , [])
  , ([PDFName "a"]               , [])
  , ([PDFName "a", PDFNumber 1.0], [])
  , ([PDFName "a", PDFName "b"]  , [])
  , ([PDFName "a", PDFName "a"]  , [])
  , ([PDFName "a", PDFName "a"]  , [])
  , ([PDFVersion "1.2", PDFVersion "1.5"], [PDFVersion "1.5"])
  , ( [PDFIndirectObject 1 0 PDFNull, PDFIndirectObject 1 0 (PDFName "a")]
    , [PDFIndirectObject 1 0 (PDFName "a")]
    )
  , ([PDFName "a", PDFVersion "1.2", PDFName "a"], [PDFVersion "1.2"])
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
  , ( [PDFIndirectObject 1 0 (PDFArray [PDFNull, PDFName "a"])]
    , predicate
    , [PDFName "a"]
    )
  , ( [ PDFIndirectObject
          1
          0
          (PDFArray [PDFNull, PDFName "a", PDFArray [PDFName "b"]])
      ]
    , predicate
    , [PDFName "a", PDFName "b"]
    )
  , ( [ PDFTrailer
          (PDFDictionary (HM.fromList [("a", PDFName "a"), ("b", PDFName "b")]))
      ]
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

  describe "clean" $ do
    forM_ cleanExamples $ \(example, expected) ->
      it ("should give " ++ show expected ++ " for " ++ show example)
        $          (clean . fromList) example
        `shouldBe` fromList expected

  describe "deepFind" $ do
    forM_ deepFindExamples $ \(example, predicate, expected) ->
      it ("should work with " ++ show example)
        $          (deepFind predicate . fromList) example
        `shouldBe` fromList expected
