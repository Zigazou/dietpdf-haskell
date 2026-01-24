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
  , ( "497 0 obj\x0D<</BBox[0.0 841.89 566.929 0.0]/Filter/FlateDecode/Length 9\
      \/Matrix[1.0 0.0 0.0 1.0 0.0 0.0]/Resources<<>>/Subtype/Form>>\
      \stream\x0D\x0A\x48\x89\x02\x0C\x00\x00\x00\x00\x01\rendstream\x0D\
      \endobj"
    , PDFIndirectObjectWithStream
      497
      0
      (fromList
        [ ("BBox", PDFDictionary (fromList
            [ ("0", PDFNumber 0.0)
            , ("1", PDFNumber 841.89)
            , ("2", PDFNumber 566.929)
            , ("3", PDFNumber 0.0)
            ])
          )
        , ("Filter", PDFName "FlateDecode")
        , ("Length", PDFNumber 9.0)
        , ("Matrix", PDFDictionary (fromList
            [ ("0", PDFNumber 1.0)
            , ("1", PDFNumber 0.0)
            , ("2", PDFNumber 0.0)
            , ("3", PDFNumber 1.0)
            , ("4", PDFNumber 0.0)
            , ("5", PDFNumber 0.0)
            ])
          )
        , ("Resources", PDFDictionary mempty)
        , ("Subtype", PDFName "Form")
        ]
      )
      "\x48\x89\x02\x0C\x00\x00\x00\x00\x01"
    )
  ]

spec :: Spec
spec = describe "indirectObjectP"
  $ mapM_ (itWith "should work with " indirectObjectP) indirectObjectExamples
