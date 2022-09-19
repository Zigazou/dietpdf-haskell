{-# LANGUAGE OverloadedStrings #-}
module Pdf.Parser.ArraySpec
  ( spec
  )
where

import           Test.Hspec                     ( describe
                                                , Spec
                                                )
import qualified Data.ByteString               as BS
import           Util.ParserHelper              ( itWith )
import           Pdf.Parser.Container           ( arrayP )
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFNumber
                                                  , PDFArray
                                                  , PDFName
                                                  , PDFString
                                                  , PDFReference
                                                  )
                                                )

arrayExamples :: [(BS.ByteString, PDFObject)]
arrayExamples =
  [ ("[1   2   3]", PDFArray [PDFNumber 1.0, PDFNumber 2.0, PDFNumber 3.0])
  , ("[ 1 2 3 ]", PDFArray [PDFNumber 1.0, PDFNumber 2.0, PDFNumber 3.0])
  , ( "[/ab/cd(abc)1 2 0 R]"
    , PDFArray
      [ PDFName "ab"
      , PDFName "cd"
      , PDFString "abc"
      , PDFNumber 1.0
      , PDFReference 2 0
      ]
    )
  , ( "[[1 2][3 4]]"
    , PDFArray
      [ PDFArray [PDFNumber 1.0, PDFNumber 2.0]
      , PDFArray [PDFNumber 3.0, PDFNumber 4.0]
      ]
    )
  ]

spec :: Spec
spec =
  describe "arrayP" $ mapM_ (itWith "should work with " arrayP) arrayExamples
