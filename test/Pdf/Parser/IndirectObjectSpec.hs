{-# LANGUAGE OverloadedStrings #-}
module Pdf.Parser.IndirectObjectSpec
  ( spec
  ) where

import           Test.Hspec                     ( describe
                                                , Spec
                                                )
import qualified Data.ByteString               as BS
import           Util.ParserHelper              ( itWith )
import           Pdf.Object.Parser.IndirectObject
                                                ( indirectObjectP )
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFIndirectObject
                                                  , PDFIndirectObjectWithStream
                                                  , PDFNumber
                                                  , PDFDictionary
                                                  , PDFReference
                                                  , PDFName
                                                  , PDFArray
                                                  )
                                                )
import           Util.Dictionary                ( mkDictionary )
import           Util.Array                     ( mkArray )

indirectObjectExamples :: [(BS.ByteString, PDFObject)]
indirectObjectExamples =
  [ ( "1 0 obj<</a 1>>endobj"
    , PDFIndirectObject 1
                        0
                        (PDFDictionary (mkDictionary [("a", PDFNumber 1.0)]))
    )
  , ( "1 0 obj<</Length 8>>stream\n12345678\nendstream endobj"
    , PDFIndirectObjectWithStream 1
                                  0
                                  (mkDictionary [("Length", PDFNumber 8.0)])
                                  "12345678"
    )
  , ( "1 0 obj<</Length 1 0 R>>stream\n12345678\nendstream endobj"
    , PDFIndirectObjectWithStream
      1
      0
      (mkDictionary [("Length", PDFReference 1 0)])
      "12345678"
    )
  , ( "1 0 obj<</Length 8>>stream\r\n12345678\nendstream endobj"
    , PDFIndirectObjectWithStream 1
                                  0
                                  (mkDictionary [("Length", PDFNumber 8.0)])
                                  "12345678"
    )
  , ( "1 0 obj<</Length 8>>stream\r\n12345678endstream endobj"
    , PDFIndirectObjectWithStream 1
                                  0
                                  (mkDictionary [("Length", PDFNumber 8.0)])
                                  "12345678"
    )
  , ( "2 0 obj\n<< /Type /Page % 1\n   /Parent 1 0 R >>\nendobj"
    , PDFIndirectObject
      2
      0
      (PDFDictionary
        (mkDictionary [("Type", PDFName "Page"), ("Parent", PDFReference 1 0)])
      )
    )
  , ( "2%abcd\n0 obj\n<< /Type /Page % 1\n   /Parent 1 0 R >>\nendobj"
    , PDFIndirectObject
      2
      0
      (PDFDictionary
        (mkDictionary [("Type", PDFName "Page"), ("Parent", PDFReference 1 0)])
      )
    )
  , ( "9 0 obj <</StemV 80 /Flags 4 /Ascent 0 /FontName \
       \/BAAAAA+LiberationSerif /Type /FontDescriptor /FontFile2 7 0 R \
       \/ItalicAngle 0 /CapHeight 981 /Descent 0 \
       \/FontBBox [-543 -303 1277 981]>>\nendobj"
    , PDFIndirectObject
      9
      0
      (PDFDictionary
        (mkDictionary
          [ ("StemV"      , PDFNumber 80)
          , ("Flags"      , PDFNumber 4)
          , ("Ascent"     , PDFNumber 0)
          , ("FontName"   , PDFName "BAAAAA+LiberationSerif")
          , ("Type"       , PDFName "FontDescriptor")
          , ("FontFile2"  , PDFReference 7 0)
          , ("ItalicAngle", PDFNumber 0)
          , ("CapHeight"  , PDFNumber 981)
          , ("Descent"    , PDFNumber 0)
          , ( "FontBBox"
            , PDFArray
              $ mkArray
                  [ PDFNumber (-543)
                  , PDFNumber (-303)
                  , PDFNumber 1277
                  , PDFNumber 981
                  ]
            )
          ]
        )
      )
    )
  , ( "1 0 obj\n<</Length 10>>\nstream\n1234567890\nendstream\nendobj"
    , PDFIndirectObjectWithStream 1
                                  0
                                  (mkDictionary [("Length", PDFNumber 10)])
                                  "1234567890"
    )
  ]

spec :: Spec
spec = describe "indirectObjectP"
  $ mapM_ (itWith "should work with " indirectObjectP) indirectObjectExamples
