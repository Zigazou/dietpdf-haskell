{-# LANGUAGE OverloadedStrings #-}
module Pdf.Document.XRefSpec
  ( spec
  ) where

import           Control.Monad                  ( forM_ )
import qualified Data.Set.Ordered              as OS
import           Data.HashMap.Strict            ( fromList )
import           Pdf.Object.Object              ( PDFObject(PDFXRef)
                                                , XRefSubsection(XRefSubsection)
                                                , XRefEntry(XRefEntry)
                                                , XRefState
                                                  ( InUseEntry
                                                  , FreeEntry
                                                  )
                                                )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )

import           Pdf.Document.XRef              ( calcOffsets
                                                , xrefTable
                                                )
import           Pdf.Object.Collection          ( EncodedObject(EncodedObject)
                                                , EncodedObjects
                                                , ObjectOffsets
                                                )

calcOffsetsExamples :: [(EncodedObjects, ObjectOffsets)]
calcOffsetsExamples =
  [ ( OS.fromList
      [ EncodedObject 0 9 "%PDF-1.4\n"
      , EncodedObject 1 3 "abc"
      , EncodedObject 2 3 "def"
      ]
    , fromList [(0, 0), (1, 9), (2, 12)]
    )
  ]

xrefTableExamples :: [(EncodedObjects, PDFObject)]
xrefTableExamples =
  [ ( OS.fromList
      [ EncodedObject 0 9 "%PDF-1.4\n"
      , EncodedObject 2 3 "def"
      , EncodedObject 5 5 "ghijk"
      , EncodedObject 1 3 "abc"
      ]
    , PDFXRef
      [ XRefSubsection
          0
          6
          [ XRefEntry 0  0 InUseEntry
          , XRefEntry 9  0 InUseEntry
          , XRefEntry 12 0 InUseEntry
          , XRefEntry 0  0 FreeEntry
          , XRefEntry 0  0 FreeEntry
          , XRefEntry 15 0 InUseEntry
          ]
      ]
    )
  ]

spec :: Spec
spec = do
  describe "calcOffsets" $ forM_ calcOffsetsExamples $ \(example, expected) ->
    it ("should give right result for " ++ show example)
      $          calcOffsets example
      `shouldBe` expected

  describe "xrefTable" $ forM_ xrefTableExamples $ \(example, expected) ->
    it ("should give right result for " ++ show example)
      $          xrefTable example
      `shouldBe` expected
