{-# LANGUAGE OverloadedStrings #-}
module Pdf.Document.DocumentSpec
  ( spec
  ) where

import           Control.Monad                  ( forM_ )
import           Pdf.Document.Document          ( clean
                                                , fromList
                                                )
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFIndirectObject
                                                  , PDFName
                                                  , PDFNull
                                                  , PDFNumber
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
