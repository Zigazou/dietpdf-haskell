{-# LANGUAGE OverloadedStrings #-}
module Pdf.Object.ArraySpec
  ( spec
  )
where

import           Control.Monad                  ( forM_ )
import qualified Data.ByteString               as BS
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFArray
                                                  , PDFBool
                                                  , PDFName
                                                  , PDFNumber
                                                  )
                                                , fromPDFObject
                                                )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )

arrayExamples :: [(PDFObject, BS.ByteString)]
arrayExamples =
  [ (PDFArray [], "[]")
  , (PDFArray [PDFNumber 1.0, PDFName "AB", PDFBool True], "[1/AB true]")
  ]

spec :: Spec
spec = describe "PDFArray" $ forM_ arrayExamples $ \(example, expected) ->
  it ("should convert to bytestring " ++ show example)
    $          fromPDFObject example
    `shouldBe` expected
