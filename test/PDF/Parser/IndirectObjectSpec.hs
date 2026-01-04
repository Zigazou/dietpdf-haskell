module PDF.Parser.IndirectObjectSpec
  ( spec
  ) where

import Data.Array (mkArray)
import Data.ByteString (ByteString)

import PDF.Object.Object
    ( PDFObject (PDFArray, PDFDictionary, PDFIndirectObject, PDFIndirectObjectWithStream, PDFName, PDFNumber, PDFReference, PDFString, PDFXRefStream)
    )
import PDF.Object.Parser.IndirectObject (indirectObjectP)

import Test.Hspec (Spec, describe)

import Util.Dictionary (mkDictionary)
import Util.ParserHelper (itWith)

indirectObjectExamples :: [(ByteString, PDFObject)]
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
  , ( "11 0 obj<</Length 8>>stream\n12345678endstream endobj"
    , PDFIndirectObjectWithStream 11
                                  0
                                  (mkDictionary [("Length", PDFNumber 8.0)])
                                  "12345678"
    )
  , ( "1 0 obj\n<</Length 10>>\nstream\n1234567890\nendstream\nendobj"
    , PDFIndirectObjectWithStream 1
                                  0
                                  (mkDictionary [("Length", PDFNumber 10)])
                                  "1234567890"
    )
  , ( "186 0 obj<</Length 40/Filter/Standard/O(\xf6\x2e\x95\x40\x01\x93\x12\
      \\xf9\xc8\xae\x5c\x29\xca\xeb\x05\x48\x81\xc1\x39\x68\xe4\x48\x2c\xc4\
      \\x9d\x91\x71\xac\xe9\xb0\x59\x5c\x72\x52)/P -60/R 2/U(\
      \\x51\xc7\xde\xe8\xcf\x3d\xb6\x38\xea\x38\xca\x5e\x63\x35\x1d\x31\
      \\xe6\xc5\x73\x08\x81\x02\xbe\xba\x06\xeb\xd9\xc9\xe2\x6e\xc4\xa9)/V 1\
      \>>\rendobj"
    , PDFIndirectObject
      186
      0
      (PDFDictionary $ mkDictionary
        [ ("Length", PDFNumber 40)
        , ("Filter", PDFName "Standard")
        , ( "O"
          , PDFString
            "\xf6\x2e\x95\x40\x01\x93\x12\xf9\xc8\xae\x29\xca\xeb\x05\x48\
            \\x81\xc1\x39\x68\xe4\x48\x2c\xc4\x9d\x91\x71\xac\xe9\xb0\x59\
            \\x0d\x52"
          )
        , ("P", PDFNumber (-60))
        , ("R", PDFNumber 2)
        , ( "U"
          , PDFString
            "\x51\xc7\xde\xe8\xcf\x3d\xb6\x38\xea\x38\xca\x5e\x63\x35\x1d\x31\
            \\xe6\xc5\x73\x08\x81\x02\xbe\xba\x06\xeb\xd9\xc9\xe2\x6e\xc4\xa9"
          )
        , ("V", PDFNumber 1)
        ]
      )
    )
  , ( "12 0 obj <</Type /XRef /Length 10 >>\
      \stream\n0123456789\nendstream\nendobj"
    , PDFXRefStream
      12
      0
      (mkDictionary [("Length", PDFNumber 10), ("Type", PDFName "XRef")])
      "0123456789"
    )
  ]

spec :: Spec
spec = describe "indirectObjectP"
  $ mapM_ (itWith "should work with " indirectObjectP) indirectObjectExamples
