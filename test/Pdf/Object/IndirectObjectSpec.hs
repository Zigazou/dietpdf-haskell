module Pdf.Object.IndirectObjectSpec
  ( spec
  ) where

import           Control.Monad                  ( forM_ )
import qualified Data.ByteString               as BS
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFIndirectObject
                                                  , PDFNumber
                                                  )
                                                , fromPDFObject
                                                )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )

indirectObjectExamples :: [(PDFObject, BS.ByteString)]
indirectObjectExamples =
  [(PDFIndirectObject 1 0 (PDFNumber 1.0), "1 0 obj 1 endobj\n")]

spec :: Spec
spec =
  describe "PDFIndirectObject"
    $ forM_ indirectObjectExamples
    $ \(example, expected) ->
        it ("should convert to bytestring " ++ show example)
          $          fromPDFObject example
          `shouldBe` expected
