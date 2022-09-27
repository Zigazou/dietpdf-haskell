{-# LANGUAGE OverloadedStrings #-}
module Pdf.Document.ObjectStreamSpec
  ( spec
  ) where

import           Test.Hspec                     ( describe
                                                , it
                                                , shouldBe
                                                , Spec
                                                )
import           Control.Monad                  ( forM_ )
import           Data.HashMap.Strict            ( fromList )
import           Pdf.Document.ObjectStream      ( extract
                                                , insert
                                                )
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFIndirectObject
                                                  , PDFIndirectObjectWithStream
                                                  , PDFObjectStream
                                                  , PDFNumber
                                                  , PDFName
                                                  , PDFString
                                                  , PDFNull
                                                  )
                                                )
import           Util.Errors                    ( UnifiedError(NoObjectToEncode)
                                                )

fromObjectStreamExamples :: [(PDFObject, Either UnifiedError [PDFObject])]
fromObjectStreamExamples =
  [ ( PDFObjectStream
      1
      0
      (fromList
        [ ("Type" , PDFName "ObjStm")
        , ("N"    , PDFNumber 2)
        , ("First", PDFNumber 10)
        ]
      )
      "24 0 18 3 10 (Hello)"
    , Right
      [ PDFIndirectObject 24 0 (PDFNumber 10.0)
      , PDFIndirectObject 18 0 (PDFString "Hello")
      ]
    )
  , (PDFNull, Right [])
  ]

toObjectStreamExamples :: [([PDFObject], Either UnifiedError PDFObject)]
toObjectStreamExamples =
  [ ( [ PDFIndirectObject 24 0 (PDFNumber 10.0)
      , PDFIndirectObject 18 0 (PDFString "Hello")
      ]
    , Right $ PDFObjectStream
      1
      0
      (fromList
        [ ("Type" , PDFName "ObjStm")
        , ("N"    , PDFNumber 2)
        , ("First", PDFNumber 10)
        ]
      )
      "24 0 18 3 10 (Hello)"
    )
  , ([]       , Left NoObjectToEncode)
  , ([PDFNull], Left NoObjectToEncode)
  , ( [PDFIndirectObjectWithStream 1 0 (fromList []) "Hello, World!"]
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
