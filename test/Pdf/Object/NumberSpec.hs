module Pdf.Object.NumberSpec
  ( spec
  )
where

import Control.Monad (forM_)

import Data.ByteString qualified as BS

import Pdf.Object.Object (PDFObject (PDFNumber), fromPDFObject)

import Test.Hspec (Spec, describe, it, shouldBe)

numberExamples :: [(PDFObject, BS.ByteString)]
numberExamples =
  [ (PDFNumber 1.1   , "1.1")
  , (PDFNumber 0.0   , "0")
  , (PDFNumber 0.1   , ".1")
  , (PDFNumber (-0.1), "-.1")
  , (PDFNumber (-1.0), "-1")
  , (PDFNumber 1.0   , "1")
  ]

spec :: Spec
spec = describe "PDFNumber" $ forM_ numberExamples $ \(example, expected) ->
  it ("should convert to bytestring " ++ show example)
    $          fromPDFObject example
    `shouldBe` expected
