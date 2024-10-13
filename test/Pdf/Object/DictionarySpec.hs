module Pdf.Object.DictionarySpec
  ( spec
  ) where

import Control.Monad (forM_)

import Data.ByteString (ByteString)
import Data.PDF.PDFWork (evalPDFWorkT)

import Pdf.Object.Object
    ( PDFObject (PDFBool, PDFIndirectObject, PDFName, PDFNumber, PDFTrailer)
    , fromPDFObject
    , mkEmptyPDFDictionary
    , mkPDFArray
    , mkPDFDictionary
    )
import Pdf.Object.State (getValue)

import Test.Hspec (Spec, describe, it, shouldBe)

dictionaryExamples :: [(PDFObject, ByteString)]
dictionaryExamples =
  [ (mkEmptyPDFDictionary, "<<>>")
  , ( mkPDFDictionary
      [("AB", PDFNumber 1.0), ("CD", PDFName "AB"), ("EF", PDFBool True)]
    , "<</AB 1/CD/AB/EF true>>"
    )
  , ( mkPDFDictionary
      [ ( "AB"
        , mkPDFArray [PDFNumber 1.0, mkPDFArray [PDFNumber 2.0], PDFNumber 3.0]
        )
      , ("CD", PDFName "AB")
      , ("EF", PDFBool True)
      ]
    , "<</AB[1[2]3]/CD/AB/EF true>>"
    )
  ]

getValueExamples :: [(PDFObject, Maybe PDFObject)]
getValueExamples =
  [ (mkEmptyPDFDictionary, Nothing)
  , (PDFNumber 3.0       , Nothing)
  , ( mkPDFDictionary
      [("AB", PDFNumber 1.0), ("Test", PDFName "AB"), ("EF", PDFBool True)]
    , Just $ PDFName "AB"
    )
  , ( mkPDFDictionary
      [("AB", PDFNumber 1.0), ("CD", PDFName "AB"), ("EF", PDFBool True)]
    , Nothing
    )
  , ( PDFIndirectObject
      10
      0
      (mkPDFDictionary
        [ ( "AB"
          , mkPDFArray
            [PDFNumber 1.0, mkPDFArray [PDFNumber 2.0], PDFNumber 3.0]
          )
        , ("CD", PDFName "AB")
        , ("EF", PDFBool True)
        ]
      )
    , Nothing
    )
  , ( PDFIndirectObject
      10
      0
      (mkPDFDictionary
        [ ( "AB"
          , mkPDFArray
            [PDFNumber 1.0, mkPDFArray [PDFNumber 2.0], PDFNumber 3.0]
          )
        , ("Test", PDFNumber 4.0)
        , ("EF"  , PDFBool True)
        ]
      )
    , Just $ PDFNumber 4.0
    )
  , ( PDFTrailer
      (mkPDFDictionary
        [ ( "AB"
          , mkPDFArray
            [PDFNumber 1.0, mkPDFArray [PDFNumber 2.0], PDFNumber 3.0]
          )
        , ("Test", PDFNumber 4.0)
        , ("EF"  , PDFBool True)
        ]
      )
    , Just $ PDFNumber 4.0
    )
  ]

spec :: Spec
spec = describe "PDFDictionary" $ do
  forM_ dictionaryExamples $ \(example, expected) ->
    it ("should convert to bytestring " ++ show example)
      $          fromPDFObject example
      `shouldBe` expected

  forM_ getValueExamples $ \(example, expected) ->
    it ("should get value from Dictionary " ++ show example) $ do
      result <- evalPDFWorkT $ getValue "Test" example
      result `shouldBe` Right expected
