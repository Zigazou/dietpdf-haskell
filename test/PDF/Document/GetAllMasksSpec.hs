module PDF.Document.GetAllMasksSpec
  ( spec
  ) where

import Control.Monad (forM_)

import Data.PDF.PDFDocument (PDFDocument, fromList)
import Data.PDF.PDFObject
  ( PDFObject (PDFIndirectObject, PDFIndirectObjectWithStream, PDFReference, PDFString)
  , mkPDFDictionary
  )
import Data.Set (Set)
import Data.Set qualified as Set

import PDF.Document.GetAllMasks (getAllMasks)

import Test.Hspec (Spec, describe, it, shouldBe)

import Util.Dictionary (mkEmptyDictionary)

getAllMasksExamples :: [(PDFDocument, Set Int)]
getAllMasksExamples =
  [ ( fromList
        [ PDFIndirectObject 1 0 (PDFString "Hello, World!")
        ]
    , mempty
    )
  , ( fromList
        [ PDFIndirectObjectWithStream 2 0 mkEmptyDictionary "Hello, World!"
        ]
    , mempty
    )
  , ( fromList
        [ PDFIndirectObject 3 0 (mkPDFDictionary [
            ("Resources", mkPDFDictionary [("SMask", PDFReference 4 0)])
          ])
        ]
    , Set.fromList [4]
    )
  ]

spec :: Spec
spec = do
  describe "encodeObject" $ do
    forM_ getAllMasksExamples $ \(example, expected) ->
      it ("should encode object " ++ show example)
        $   getAllMasks example `shouldBe` expected
