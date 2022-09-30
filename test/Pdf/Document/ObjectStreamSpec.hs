{-# LANGUAGE OverloadedStrings #-}
module Pdf.Document.ObjectStreamSpec
  ( spec
  ) where

import           Control.Monad                  ( forM_ )
import qualified Data.HashMap.Strict           as HM
import           Pdf.Document.Document          ( PDFDocument
                                                , fromList
                                                )
import           Pdf.Document.ObjectStream      ( extract
                                                , insert
                                                )
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFIndirectObject
                                                  , PDFIndirectObjectWithStream
                                                  , PDFName
                                                  , PDFNull
                                                  , PDFNumber
                                                  , PDFObjectStream
                                                  , PDFString
                                                  )
                                                )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )
import           Util.Errors                    ( UnifiedError(NoObjectToEncode)
                                                )

fromObjectStreamExamples :: [(PDFObject, Either UnifiedError PDFDocument)]
fromObjectStreamExamples =
  [ ( PDFObjectStream
      1
      0
      (HM.fromList
        [ ("Type" , PDFName "ObjStm")
        , ("N"    , PDFNumber 2)
        , ("First", PDFNumber 10)
        ]
      )
      "24 0 18 3 10 (Hello)"
    , Right $ fromList
      [ PDFIndirectObject 24 0 (PDFNumber 10.0)
      , PDFIndirectObject 18 0 (PDFString "Hello")
      ]
    )
  , (PDFNull, Right mempty)
  ]

toObjectStreamExamples :: [(PDFDocument, Either UnifiedError PDFObject)]
toObjectStreamExamples =
  [ ( fromList
      [ PDFIndirectObject 24 0 (PDFNumber 10.0)
      , PDFIndirectObject 18 0 (PDFString "Hello")
      ]
    , Right $ PDFObjectStream
      1
      0
      (HM.fromList
        [ ("Type" , PDFName "ObjStm")
        , ("N"    , PDFNumber 2)
        , ("First", PDFNumber 10)
        ]
      )
      "24 0 18 3 10 (Hello)"
    )
  , (fromList []       , Left NoObjectToEncode)
  , (fromList [PDFNull], Left NoObjectToEncode)
  , ( fromList
      [PDFIndirectObjectWithStream 1 0 (HM.fromList []) "Hello, World!"]
    , Left NoObjectToEncode
    )
  ]

spec :: Spec
spec = do
  describe "extract" $ do
    forM_ fromObjectStreamExamples $ \(example, expected) ->
      it ("should decode " ++ show example)
        $          extract example
        `shouldBe` expected

  describe "insert" $ do
    forM_ toObjectStreamExamples $ \(example, expected) ->
      it ("should encode " ++ show example)
        $          insert example 1
        `shouldBe` expected
