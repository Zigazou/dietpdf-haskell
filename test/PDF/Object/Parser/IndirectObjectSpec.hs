module PDF.Object.Parser.IndirectObjectSpec
  ( spec
  ) where

import Data.ByteString (ByteString)
import Data.Map.Strict (fromList)

import PDF.Object.Object
    ( PDFObject (PDFDictionary, PDFIndirectObject, PDFIndirectObjectWithStream, PDFName, PDFNumber, PDFReference)
    )
import PDF.Object.Parser.IndirectObject (indirectObjectP)

import Test.Hspec (Spec, describe)

import Util.ParserHelper (itWith)

indirectObjectExamples :: [(ByteString, PDFObject)]
indirectObjectExamples =
  [ ( "1 0 obj<</a 1>>endobj"
    , PDFIndirectObject 1 0 (PDFDictionary (fromList [("a", PDFNumber 1.0)]))
    )
  , ( "1 0 obj<</Length 8>>stream\n12345678\nendstream endobj"
    , PDFIndirectObjectWithStream 1
                                  0
                                  (fromList [("Length", PDFNumber 8.0)])
                                  "12345678"
    )
  , ( "1 0 obj<</Length 1 0 R>>stream\n12345678\nendstream endobj"
    , PDFIndirectObjectWithStream 1
                                  0
                                  (fromList [("Length", PDFReference 1 0)])
                                  "12345678"
    )
  , ( "1 0 obj<</Length 8>>stream\r\n12345678\nendstream endobj"
    , PDFIndirectObjectWithStream 1
                                  0
                                  (fromList [("Length", PDFNumber 8.0)])
                                  "12345678"
    )
  , ( "2 0 obj\n<< /Type /Page % 1\n   /Parent 1 0 R >>\nendobj"
    , PDFIndirectObject
      2
      0
      (PDFDictionary
        (fromList [("Type", PDFName "Page"), ("Parent", PDFReference 1 0)])
      )
    )
  , ( "2%abcd\n0 obj\n<< /Type /Page % 1\n   /Parent 1 0 R >>\nendobj"
    , PDFIndirectObject
      2
      0
      (PDFDictionary
        (fromList [("Type", PDFName "Page"), ("Parent", PDFReference 1 0)])
      )
    )
  , ( "219 0 obj<</Length 289/B 314/E 282/Filter/FlateDecode/I 338/L 298/S 183\
      \>>stream\r\n***********************************************************\
      \***********************************************************************\
      \***********************************************************************\
      \***********************************************************************\
      \*****************\r\nendstream\rendobj"
    , PDFIndirectObjectWithStream
      219
      0
      (fromList
        [ ("Length", PDFNumber 289.0)
        , ("B"     , PDFNumber 314.0)
        , ("E"     , PDFNumber 282.0)
        , ("Filter", PDFName "FlateDecode")
        , ("I"     , PDFNumber 338.0)
        , ("L"     , PDFNumber 298.0)
        , ("S"     , PDFNumber 183.0)
        ]
      )
      "***********************************************************\
      \***********************************************************************\
      \***********************************************************************\
      \***********************************************************************\
      \*****************"
    )
  ]

spec :: Spec
spec = describe "indirectObjectP"
  $ mapM_ (itWith "should work with " indirectObjectP) indirectObjectExamples
