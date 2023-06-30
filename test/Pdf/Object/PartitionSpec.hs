{-# LANGUAGE OverloadedStrings #-}
module Pdf.Object.PartitionSpec
  ( spec
  ) where

import           Control.Monad                  ( forM_ )
import qualified Data.Map.Strict           as Map
import           Pdf.Document.Document          ( fromList )
import           Pdf.Document.Partition         ( PDFPartition(PDFPartition)
                                                , toPartition
                                                )
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFComment
                                                  , PDFDictionary
                                                  , PDFEndOfFile
                                                  , PDFIndirectObject
                                                  , PDFIndirectObjectWithStream
                                                  , PDFName
                                                  , PDFNumber
                                                  , PDFObjectStream
                                                  , PDFStartXRef
                                                  , PDFTrailer
                                                  , PDFVersion
                                                  , PDFXRef
                                                  )
                                                )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )

toPartitionExamples :: [([PDFObject], PDFPartition)]
toPartitionExamples =
  [ ( [ PDFVersion "1.7"
      , PDFComment "foo"
      , PDFIndirectObject 1 0 (PDFName "bar")
      , PDFIndirectObjectWithStream 3
                                    0
                                    (Map.fromList [("Name", PDFName "bar")])
                                    "def"
      , PDFIndirectObjectWithStream 4
                                    0
                                    (Map.fromList [("Name", PDFName "foo")])
                                    "abc"
      , PDFObjectStream 5 0 (Map.fromList [("Name", PDFName "baz")]) "xyz"
      , PDFXRef []
      , PDFTrailer
        (PDFDictionary
          (Map.fromList [("Size", PDFNumber 2000.0), ("Root", PDFNumber 1.0)])
        )
      , PDFStartXRef 1500
      , PDFEndOfFile
      ]
    , PDFPartition
      (fromList
        [ PDFIndirectObject 1 0 (PDFName "bar")
        , PDFIndirectObjectWithStream 3
                                      0
                                      (Map.fromList [("Name", PDFName "bar")])
                                      "def"
        , PDFIndirectObjectWithStream 4
                                      0
                                      (Map.fromList [("Name", PDFName "foo")])
                                      "abc"
        , PDFObjectStream 5 0 (Map.fromList [("Name", PDFName "baz")]) "xyz"
        ]
      )
      (fromList [PDFVersion "1.7"])
      (fromList
        [ PDFTrailer
            (PDFDictionary
              (Map.fromList [("Size", PDFNumber 2000.0), ("Root", PDFNumber 1.0)]
              )
            )
        ]
      )
    )
  ]

spec :: Spec
spec =
  describe "toPartition" $ forM_ toPartitionExamples $ \(example, expected) ->
    it ("should give right result for " ++ show example)
      $          mconcat (toPartition <$> example)
      `shouldBe` expected
