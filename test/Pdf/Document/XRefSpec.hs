module Pdf.Document.XRefSpec
  ( spec
  ) where

import Control.Monad (forM_)

import Data.IntMap.Strict qualified as IM

import Pdf.Document.Collection
    ( EncodedObject (EncodedObject)
    , EncodedObjects
    , ObjectOffsets
    )
import Pdf.Document.XRef (calcOffsets, xrefStreamTable, xrefTable)
import Pdf.Object.Object
    ( PDFObject (PDFName, PDFXRef, PDFXRefStream)
    , ToPDFNumber (mkPDFNumber)
    , XRefEntry (XRefEntry)
    , XRefState (FreeEntry, InUseEntry)
    , XRefSubsection (XRefSubsection)
    , mkPDFArray
    )

import Test.Hspec (Spec, describe, it, shouldBe)

import Util.Dictionary (mkDictionary)

calcOffsetsExamples :: [(EncodedObjects, ObjectOffsets)]
calcOffsetsExamples =
  [ ( IM.fromList
      [ (0, EncodedObject 0 9 "%PDF-1.4\n")
      , (1, EncodedObject 1 3 "abc")
      , (2, EncodedObject 2 3 "def")
      ]
    , IM.fromList [(0, 0), (1, 9), (2, 12)]
    )
  ]

xrefTableExamples :: [(EncodedObjects, PDFObject)]
xrefTableExamples =
  [ ( IM.fromList
      [ (0, EncodedObject 0 9 "%PDF-1.4\n")
      , (2, EncodedObject 2 3 "def")
      , (5, EncodedObject 5 5 "ghijk")
      , (1, EncodedObject 1 3 "abc")
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

xrefStreamTableExamples :: [(EncodedObjects, PDFObject)]
xrefStreamTableExamples =
  [ ( IM.fromList
      [ (2, EncodedObject 2 3 "def")
      , (5, EncodedObject 5 5 "ghijk")
      , (1, EncodedObject 1 3 "abc")
      ]
    , PDFXRefStream
      10
      0
      (mkDictionary
        [ ("Type", PDFName "XRef")
        , ( "W"
          , mkPDFArray
            [ mkPDFNumber (1 :: Double)
            , mkPDFNumber (1 :: Double)
            , mkPDFNumber (1 :: Double)
            ]
          )
        , ( "Index"
          , mkPDFArray [mkPDFNumber (1 :: Double), mkPDFNumber (3 :: Double)]
          )
        , ("Size", mkPDFNumber (9 :: Double))
        ]
      )
      "\x01\x00\x01\
      \\x01\x03\x02\
      \\x01\x06\x05"
    )
  ]

req :: PDFObject -> PDFObject -> Bool
req (PDFXRefStream x1 y1 d1 s1) (PDFXRefStream x2 y2 d2 s2) =
  x1 == x2 && y1 == y2 && d1 == d2 && s1 == s2
req _ _ = False

spec :: Spec
spec = do
  describe "calcOffsets" $ forM_ calcOffsetsExamples $ \(example, expected) ->
    it ("should give right result for " ++ show example)
      $          calcOffsets 0 example
      `shouldBe` expected

  describe "xrefTable" $ forM_ xrefTableExamples $ \(example, expected) ->
    it ("should give right result for " ++ show example)
      $          xrefTable 0 example
      `shouldBe` expected

  describe "xrefStreamTable"
    $ forM_ xrefStreamTableExamples
    $ \(example, expected) -> do
        let actual = xrefStreamTable 10 0 example
        it
            (  "should give right result for "
            ++ show actual
            ++ " --> "
            ++ show expected
            )
          $ shouldBe (req actual expected) True
