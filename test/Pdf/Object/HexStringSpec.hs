module Pdf.Object.HexStringSpec
  ( spec
  )
where

import           Control.Monad                  ( forM_ )
import qualified Data.ByteString               as BS
import           Pdf.Object.Object              ( PDFObject(PDFHexString)
                                                , fromPDFObject
                                                )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )

hexStringExamples :: [(PDFObject, BS.ByteString)]
hexStringExamples =
  [ (PDFHexString "123456"     , "<123456>")
  , (PDFHexString ""           , "<>")
  , (PDFHexString "1"          , "<1>")
  , (PDFHexString "10"         , "<1>")
  , (PDFHexString "0"          , "<0>")
  , (PDFHexString "00"         , "<0>")
  , (PDFHexString "ABCDEF"     , "<abcdef>")
  , (PDFHexString "a b c d e f", "<abcdef>")
  , (PDFHexString "1 0"        , "<1>")
  ]

spec :: Spec
spec =
  describe "PDFHexString" $ forM_ hexStringExamples $ \(example, expected) ->
    it ("should convert to bytestring " ++ show example)
      $          fromPDFObject example
      `shouldBe` expected
