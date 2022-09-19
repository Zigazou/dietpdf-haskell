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
      , PDFIndirectObject 1 0 (PDFName "bar") Nothing
      , PDFIndirectObject
        4
        0
        (PDFDictionary (HM.fromList [("Name", PDFName "foo")]))
        (Just "abc")
      , PDFIndirectObject
        3
        0
        (PDFDictionary (HM.fromList [("Name", PDFName "bar")]))
        (Just "def")
      , PDFXRef []
      , PDFTrailer
        (PDFDictionary
          (HM.fromList [("Size", PDFNumber 2000.0), ("Root", PDFNumber 1.0)])
        )
      , PDFStartXRef 1500
      , PDFEndOfFile
      ]
    , PDFPartition
      [ PDFIndirectObject 1 0 (PDFName "bar") Nothing
      , PDFIndirectObject
        4
        0
        (PDFDictionary (HM.fromList [("Name", PDFName "foo")]))
        (Just "abc")
      , PDFIndirectObject
        3
        0
        (PDFDictionary (HM.fromList [("Name", PDFName "bar")]))
        (Just "def")
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
