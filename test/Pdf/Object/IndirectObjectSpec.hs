module Pdf.Object.IndirectObjectSpec
  ( spec
  ) where

import Control.Monad (forM_)

import Data.ByteString qualified as BS

import Pdf.Object.Object
    ( PDFObject (PDFIndirectObject, PDFNumber)
    , fromPDFObject
    )

import Test.Hspec (Spec, describe, it, shouldBe)

indirectObjectExamples :: [(PDFObject, BS.ByteString)]
indirectObjectExamples =
  [(PDFIndirectObject 1 0 (PDFNumber 1.0), "1 0 obj\n1\nendobj\n")]

spec :: Spec
spec =
  describe "PDFIndirectObject"
    $ forM_ indirectObjectExamples
    $ \(example, expected) ->
        it ("should convert to bytestring " ++ show example)
          $          fromPDFObject example
          `shouldBe` expected
