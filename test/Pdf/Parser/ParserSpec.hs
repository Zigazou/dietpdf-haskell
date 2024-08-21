module Pdf.Parser.ParserSpec
  ( spec
  ) where

import Control.Monad (forM_)
import Control.Monad.Trans.Except (runExceptT)

import Data.ByteString qualified as BS

import Pdf.Document.Document (PDFDocument, fromList)
import Pdf.Document.Parser (pdfParse)
import Pdf.Object.Object
    ( PDFObject (PDFArray, PDFDictionary, PDFEndOfFile, PDFHexString, PDFIndirectObject, PDFNumber, PDFReference, PDFTrailer, PDFVersion, PDFXRef)
    , XRefEntry (XRefEntry)
    , XRefState (InUseEntry)
    , XRefSubsection (XRefSubsection)
    )

import Test.Hspec (Spec, describe, it, shouldBe)

import Util.Array (mkArray)
import Util.Dictionary (mkDictionary)
import Util.UnifiedError (UnifiedError)

pdfParseExamples :: [(BS.ByteString, Either UnifiedError PDFDocument)]
pdfParseExamples =
  [ ("%PDF-1.4\n", Right $ fromList [PDFVersion "1.4"])
  , ( "%PDF-1.4\n      \n%%EOF\n"
    , Right $ fromList [PDFVersion "1.4", PDFEndOfFile]
    )
  , ("%%EOF\n", Right $ fromList [PDFEndOfFile])
  , ( "xref\n1 1\n0000000000 00000 n \ntrailer\n\n<<\n/Info 675 0 R\n\
     \/ID [<dfeef40d72cc1a237c43702126fcacea><fe2ccdc64c1f9e903d5ef1384c263447>\
     \]\n/Root 674 0 R\n/Size 676\n>>"
    , Right $ fromList
      [ PDFXRef [XRefSubsection 1 1 [XRefEntry 0 0 InUseEntry]]
      , PDFTrailer
        (PDFDictionary
          (mkDictionary
            [ ("Info", PDFReference 675 0)
            , ("Root", PDFReference 674 0)
            , ("Size", PDFNumber 676)
            , ( "ID"
              , PDFArray $ mkArray
                [ PDFHexString "dfeef40d72cc1a237c43702126fcacea"
                , PDFHexString "fe2ccdc64c1f9e903d5ef1384c263447"
                ]
              )
            ]
          )
        )
      ]
    )
  , ( "%PDF-1.4\n1 0 obj<</ID 3>>\nendobj\n%%EOF\n2 0 obj<</ID 4>>\nendobj"
    , Right $ fromList
      [ PDFVersion "1.4"
      , PDFIndirectObject 1
                          0
                          (PDFDictionary $ mkDictionary [("ID", PDFNumber 3)])
      , PDFEndOfFile
      , PDFIndirectObject 2
                          0
                          (PDFDictionary $ mkDictionary [("ID", PDFNumber 4)])
      ]
    )
  , ( "%PDF-1.4\r\n     \r\n1 0 obj<</ID 3>>\nendobj\n      \n%%EOF\n\
      \2 0 obj<</ID 4>>\nendobj"
    , Right $ fromList
      [ PDFVersion "1.4"
      , PDFIndirectObject 1
                          0
                          (PDFDictionary $ mkDictionary [("ID", PDFNumber 3)])
      , PDFEndOfFile
      , PDFIndirectObject 2
                          0
                          (PDFDictionary $ mkDictionary [("ID", PDFNumber 4)])
      ]
    )
  ]

spec :: Spec
spec = describe "pdfParse" $ forM_ pdfParseExamples $ \(example, expected) ->
  it "should work with various pieces of PDF" $ do
    result <- runExceptT (pdfParse example)
    result `shouldBe` expected
