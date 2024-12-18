module PDF.Object.ReferenceSpec
  ( spec
  )
where

import Control.Monad (forM_)

import Data.ByteString (ByteString)

import PDF.Object.Object (PDFObject (PDFReference), fromPDFObject)

import Test.Hspec (Spec, describe, it, shouldBe)

referenceExamples :: [(PDFObject, ByteString)]
referenceExamples =
  [(PDFReference 1 0, "1 0 R"), (PDFReference 20 20, "20 20 R")]

spec :: Spec
spec =
  describe "PDFReference" $ forM_ referenceExamples $ \(example, expected) ->
    it ("should convert to bytestring " ++ show example)
      $          fromPDFObject example
      `shouldBe` expected
