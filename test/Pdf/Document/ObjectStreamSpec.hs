{-# LANGUAGE OverloadedStrings #-}
module Pdf.Document.ObjectStreamSpec
  ( spec
  ) where

import           Control.Monad                  ( forM_ )
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
import           Util.Dictionary                ( mkDictionary
                                                , mkEmptyDictionary
                                                )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )
import           Util.UnifiedError              ( UnifiedError(NoObjectToEncode, ObjectStreamNotFound)
                                                , Fallible
                                                )
import           Control.Monad.Trans.Except     ( runExceptT )

fromObjectStreamExamples :: [(PDFObject, Fallible PDFDocument)]
fromObjectStreamExamples =
  [ ( PDFObjectStream
      1
      0
      (mkDictionary
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
  , (PDFNull, Left ObjectStreamNotFound)
  ]

toObjectStreamExamples :: [(PDFDocument, Fallible PDFObject)]
toObjectStreamExamples =
  [ ( fromList
      [ PDFIndirectObject 24 0 (PDFNumber 10.0)
      , PDFIndirectObject 18 0 (PDFString "Hello")
      ]
    , Right $ PDFObjectStream
      1
      0
      (mkDictionary
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
      [PDFIndirectObjectWithStream 1 0 mkEmptyDictionary "Hello, World!"]
    , Left NoObjectToEncode
    )
  ]

spec :: Spec
spec = do
  describe "extract" $ do
    forM_ fromObjectStreamExamples $ \(example, expected) ->
      it ("should decode " ++ show example)
        $   runExceptT (extract example)
        >>= (`shouldBe` expected)

  describe "insert" $ do
    forM_ toObjectStreamExamples $ \(example, expected) ->
      it ("should encode " ++ show example)
        $   runExceptT (insert example 1)
        >>= (`shouldBe` expected)
