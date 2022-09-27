{-# LANGUAGE OverloadedStrings #-}
module Pdf.Object.PartitionSpec
  ( spec
  ) where

import           Control.Monad                  ( forM_ )
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFVersion
                                                  , PDFEndOfFile
                                                  , PDFXRef
                                                  , PDFNumber
                                                  , PDFStartXRef
                                                  , PDFTrailer
                                                  , PDFComment
                                                  , PDFDictionary
                                                  , PDFIndirectObject
                                                  , PDFIndirectObjectWithStream
                                                  , PDFObjectStream
                                                  , PDFName
                                                  )
                                                )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )
import qualified Data.HashMap.Strict           as HM
import           Pdf.Object.Partition           ( toPartition
                                                , PDFPartition(PDFPartition)
                                                )

toPartitionExamples :: [([PDFObject], PDFPartition)]
toPartitionExamples =
  [ ( [ PDFVersion "1.7"
      , PDFComment "foo"
      , PDFIndirectObject 1 0 (PDFName "bar")
      , PDFIndirectObjectWithStream 3
                                    0
                                    (HM.fromList [("Name", PDFName "bar")])
                                    "def"
      , PDFIndirectObjectWithStream 4
                                    0
                                    (HM.fromList [("Name", PDFName "foo")])
                                    "abc"
      , PDFObjectStream 5 0 (HM.fromList [("Name", PDFName "baz")]) "xyz"
      , PDFXRef []
      , PDFTrailer
        (PDFDictionary
          (HM.fromList [("Size", PDFNumber 2000.0), ("Root", PDFNumber 1.0)])
        )
      , PDFStartXRef 1500
      , PDFEndOfFile
      ]
    , PDFPartition
      [ PDFIndirectObject 1 0 (PDFName "bar")
      , PDFIndirectObjectWithStream 3
                                    0
                                    (HM.fromList [("Name", PDFName "bar")])
                                    "def"
      , PDFIndirectObjectWithStream 4
                                    0
                                    (HM.fromList [("Name", PDFName "foo")])
                                    "abc"
      , PDFObjectStream 5 0 (HM.fromList [("Name", PDFName "baz")]) "xyz"
      ]
      [PDFVersion "1.7"]
      [ PDFTrailer
          (PDFDictionary
            (HM.fromList [("Size", PDFNumber 2000.0), ("Root", PDFNumber 1.0)])
          )
      ]
    )
  ]

spec :: Spec
spec =
  describe "toPartition" $ forM_ toPartitionExamples $ \(example, expected) ->
    it ("should give right result for " ++ show example)
      $          mconcat (toPartition <$> example)
      `shouldBe` expected
