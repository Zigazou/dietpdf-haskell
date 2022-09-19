{-# LANGUAGE OverloadedStrings #-}
module Pdf.Parser.IndirectObjectSpec
  ( spec
  ) where

import           Test.Hspec                     ( describe
                                                , Spec
                                                )
import qualified Data.ByteString               as BS
import           Data.HashMap.Strict            ( fromList )
import           Util.ParserHelper              ( itWith )
import           Pdf.Parser.IndirectObject      ( indirectObjectP )
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFIndirectObject
                                                  , PDFNumber
                                                  , PDFDictionary
                                                  , PDFReference
                                                  , PDFName
                                                  )
                                                )


indirectObjectExamples :: [(BS.ByteString, PDFObject)]
indirectObjectExamples =
  [ ( "1 0 obj<</a 1>>endobj"
    , PDFIndirectObject 1
                        0
                        (PDFDictionary (fromList [("a", PDFNumber 1.0)]))
                        Nothing
    )
  , ( "1 0 obj<</Length 8>>stream\n12345678\nendstream endobj"
    , PDFIndirectObject 1
                        0
                        (PDFDictionary (fromList [("Length", PDFNumber 8.0)]))
                        (Just "12345678")
    )
  , ( "1 0 obj<</Length 1 0 R>>stream\n12345678\nendstream endobj"
    , PDFIndirectObject
      1
      0
      (PDFDictionary (fromList [("Length", PDFReference 1 0)]))
      (Just "12345678")
    )
  , ( "1 0 obj<</Length 8>>stream\r\n12345678\nendstream endobj"
    , PDFIndirectObject 1
                        0
                        (PDFDictionary (fromList [("Length", PDFNumber 8.0)]))
                        (Just "12345678")
    )
  , ( "1 0 obj<</Length 8>>stream\r\n12345678endstream endobj"
    , PDFIndirectObject 1
                        0
                        (PDFDictionary (fromList [("Length", PDFNumber 8.0)]))
                        (Just "12345678")
    )
  , ( "2 0 obj\n<< /Type /Page % 1\n   /Parent 1 0 R >>\nendobj"
    , PDFIndirectObject
      2
      0
      (PDFDictionary
        (fromList [("Type", PDFName "Page"), ("Parent", PDFReference 1 0)])
      )
      Nothing
    )
  , ( "2%abcd\n0 obj\n<< /Type /Page % 1\n   /Parent 1 0 R >>\nendobj"
    , PDFIndirectObject
      2
      0
      (PDFDictionary
        (fromList [("Type", PDFName "Page"), ("Parent", PDFReference 1 0)])
      )
      Nothing
    )
  ]

spec :: Spec
spec = describe "indirectObjectP"
  $ mapM_ (itWith "should work with " indirectObjectP) indirectObjectExamples
