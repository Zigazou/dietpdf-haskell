module PDF.Object.ArraySpec
  ( spec
  ) where

import Control.Monad (forM_)

import Data.ByteString (ByteString)

import PDF.Object.Object
    ( PDFObject (PDFBool, PDFName, PDFNumber)
    , fromPDFObject
    , mkEmptyPDFArray
    , mkPDFArray
    )

import Test.Hspec (Spec, describe, it, shouldBe)

arrayExamples :: [(PDFObject, ByteString)]
arrayExamples =
  [ (mkEmptyPDFArray, "[]")
  , (mkPDFArray [PDFNumber 1.0, PDFName "AB", PDFBool True], "[1/AB true]")
  ]

spec :: Spec
spec = describe "PDFArray" $ forM_ arrayExamples $ \(example, expected) ->
  it ("should convert to bytestring " ++ show example)
    $          fromPDFObject example
    `shouldBe` expected
