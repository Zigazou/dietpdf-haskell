module PDF.Object.Parser.ArraySpec
  ( spec
  ) where

import Data.ByteString (ByteString)

import PDF.Object.Object
    ( PDFObject (PDFName, PDFNumber, PDFReference, PDFString)
    , mkPDFArray
    )
import PDF.Object.Parser.Container (arrayP)

import Test.Hspec (Spec, describe)

import Util.ParserHelper (itWith)

arrayExamples :: [(ByteString, PDFObject)]
arrayExamples =
  [ ("[1   2   3]", mkPDFArray [PDFNumber 1.0, PDFNumber 2.0, PDFNumber 3.0])
  , ("[ 1 2 3 ]"  , mkPDFArray [PDFNumber 1.0, PDFNumber 2.0, PDFNumber 3.0])
  , ( "[/ab/cd(abc)1 2 0 R]"
    , mkPDFArray
      [ PDFName "ab"
      , PDFName "cd"
      , PDFString "abc"
      , PDFNumber 1.0
      , PDFReference 2 0
      ]
    )
  , ( "[[1 2][3 4]]"
    , mkPDFArray
      [ mkPDFArray [PDFNumber 1.0, PDFNumber 2.0]
      , mkPDFArray [PDFNumber 3.0, PDFNumber 4.0]
      ]
    )
  ]

spec :: Spec
spec =
  describe "arrayP" $ mapM_ (itWith "should work with " arrayP) arrayExamples
