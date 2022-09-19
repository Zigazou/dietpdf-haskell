{-# LANGUAGE OverloadedStrings #-}
module Pdf.Object.CollectionSpec
  ( spec
  ) where

import           Control.Monad                  ( forM_ )
import           Data.HashMap.Strict            ( fromList )
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFNumber
                                                  , PDFName
                                                  , PDFDictionary
                                                  )
                                                )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )

import           Pdf.Object.Collection          ( findLastObject
                                                , findLastValue
                                                )

findLastObjectExamples :: [([PDFObject], PDFObject -> Bool, Maybe PDFObject)]
findLastObjectExamples =
  [ ( [PDFDictionary (fromList []), PDFNumber 3.0, PDFName "ABCD"]
    , isPDFNumberGreaterThan 3
    , Nothing
    )
  , ( [ PDFDictionary (fromList [])
      , PDFNumber 3.0
      , PDFName "ABCD"
      , PDFNumber 4.0
      ]
    , isPDFNumber
    , Just $ PDFNumber 4.0
    )
  ]
 where
  isPDFNumber :: PDFObject -> Bool
  isPDFNumber (PDFNumber _) = True
  isPDFNumber _             = False

  isPDFNumberGreaterThan :: Double -> PDFObject -> Bool
  isPDFNumberGreaterThan mini (PDFNumber value) = value > mini
  isPDFNumberGreaterThan _    _                 = False


findLastValueExamples
  :: [([PDFObject], PDFObject -> Maybe Double, Maybe Double)]
findLastValueExamples =
  [ ( [PDFDictionary (fromList []), PDFNumber 3.0, PDFName "ABCD"]
    , isPDFNumberGreaterThan 3
    , Nothing
    )
  , ( [ PDFDictionary (fromList [])
      , PDFNumber 3.0
      , PDFName "ABCD"
      , PDFNumber 4.0
      ]
    , isPDFNumber
    , Just 4.0
    )
  ]
 where
  isPDFNumber :: PDFObject -> Maybe Double
  isPDFNumber (PDFNumber value) = Just value
  isPDFNumber _                 = Nothing

  isPDFNumberGreaterThan :: Double -> PDFObject -> Maybe Double
  isPDFNumberGreaterThan mini (PDFNumber value) =
    if value > mini then Just value else Nothing
  isPDFNumberGreaterThan _ _ = Nothing

spec :: Spec
spec = do
  describe "findLastObject"
    $ forM_ findLastObjectExamples
    $ \(example, predicate, expected) ->
        it ("should give right result for " ++ show example)
          $          findLastObject predicate example
          `shouldBe` expected

  describe "findLastValue"
    $ forM_ findLastValueExamples
    $ \(example, predicate, expected) ->
        it ("should give right result for " ++ show example)
          $          findLastValue predicate example
          `shouldBe` expected
