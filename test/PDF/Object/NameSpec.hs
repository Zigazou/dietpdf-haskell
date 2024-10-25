module PDF.Object.NameSpec
  ( spec
  )
where

import Control.Monad (forM_)

import Data.ByteString (ByteString)

import PDF.Object.Object (PDFObject (PDFName), fromPDFObject)

import Test.Hspec (Spec, describe, it, shouldBe)

nameExamples :: [(PDFObject, ByteString)]
nameExamples =
  [ (PDFName "A#B"  , "/A#23B")
  , (PDFName "A B C", "/A#20B#20C")
  , (PDFName "A\nB" , "/A#0AB")
  ]

spec :: Spec
spec = describe "PDFName" $ forM_ nameExamples $ \(example, expected) ->
  it ("should convert to bytestring " ++ show example)
    $          fromPDFObject example
    `shouldBe` expected
