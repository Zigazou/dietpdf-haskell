module Pdf.Object.ArraySpec
  ( spec
  ) where

import           Control.Monad                  ( forM_ )
import qualified Data.ByteString               as BS
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFBool
                                                  , PDFName
                                                  , PDFNumber
                                                  )
                                                , mkPDFArray
                                                , mkEmptyPDFArray
                                                , fromPDFObject
                                                )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )

arrayExamples :: [(PDFObject, BS.ByteString)]
arrayExamples =
  [ (mkEmptyPDFArray, "[]")
  , (mkPDFArray [PDFNumber 1.0, PDFName "AB", PDFBool True], "[1/AB true]")
  ]

spec :: Spec
spec = describe "PDFArray" $ forM_ arrayExamples $ \(example, expected) ->
  it ("should convert to bytestring " ++ show example)
    $          fromPDFObject example
    `shouldBe` expected
