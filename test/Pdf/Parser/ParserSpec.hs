{-# LANGUAGE OverloadedStrings #-}
module Pdf.Parser.ParserSpec
  ( spec
  ) where

import           Test.Hspec                     ( describe
                                                , it
                                                , Spec
                                                , shouldBe
                                                )
import           Control.Monad                  ( forM_ )
import qualified Data.ByteString               as BS
import           Pdf.Parser.Parser              ( pdfParse )
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFIndirectObject
                                                  , PDFEndOfFile
                                                  , PDFVersion
                                                  , PDFDictionary
                                                  , PDFNumber
                                                  , PDFXRef
                                                  , PDFTrailer
                                                  , PDFReference
                                                  , PDFHexString
                                                  , PDFArray
                                                  )
                                                , XRefSubsection(XRefSubsection)
                                                , XRefEntry(XRefEntry)
                                                , XRefState(InUseEntry)
                                                )
import           Util.UnifiedError                    ( UnifiedError )
import qualified Data.HashMap.Strict           as HM

pdfParseExamples :: [(BS.ByteString, Either UnifiedError [PDFObject])]
pdfParseExamples =
  [ ("%PDF-1.4\n", Right [PDFVersion "1.4"])
  , ("%%EOF\n"   , Right [PDFEndOfFile])
  , ( "xref\n1 1\n0000000000 00000 n \ntrailer\n\n<<\n/Info 675 0 R\n\
     \/ID [<dfeef40d72cc1a237c43702126fcacea><fe2ccdc64c1f9e903d5ef1384c263447>\
     \]\n/Root 674 0 R\n/Size 676\n>>\n"
    , Right
      [ PDFXRef [XRefSubsection 1 1 [XRefEntry 0 0 InUseEntry]]
      , PDFTrailer
        (PDFDictionary
          (HM.fromList
            [ ("Info", PDFReference 675 0)
            , ("Root", PDFReference 674 0)
            , ("Size", PDFNumber 676)
            , ( "ID"
              , PDFArray
                [ PDFHexString "dfeef40d72cc1a237c43702126fcacea"
                , PDFHexString "fe2ccdc64c1f9e903d5ef1384c263447"
                ]
              )
            ]
          )
        )
      ]
    )
  , ( "%PDF-1.4\n1 0 obj<</ID 3>>\nendobj\n%%EOF\n2 0 obj<</ID 4>>\nendobj\n"
    , Right
      [ PDFVersion "1.4"
      , PDFIndirectObject 1
                          0
                          (PDFDictionary $ HM.fromList [("ID", PDFNumber 3)])
                          Nothing
      , PDFEndOfFile
      , PDFIndirectObject 2
                          0
                          (PDFDictionary $ HM.fromList [("ID", PDFNumber 4)])
                          Nothing
      ]
    )
  ]

spec :: Spec
spec = describe "pdfParse" $ forM_ pdfParseExamples $ \(example, expected) ->
  it "should work with various pieces of PDF" $ do
    pdfParse example `shouldBe` expected

