module Pdf.Object.StringSpec
  ( spec
  ) where

import Control.Monad (forM_)
import Control.Monad.Trans.Except (runExceptT)

import Data.ByteString qualified as BS

import Pdf.Object.Object (PDFObject (PDFHexString, PDFString), fromPDFObject)
import Pdf.Object.String (optimizeString)

import Test.Hspec (Spec, describe, it, shouldBe)

stringExamples :: [(PDFObject, BS.ByteString)]
stringExamples =
  [ (PDFString ""             , "()")
  , (PDFString "()"           , "(\\(\\))")
  , (PDFString "(((\\"        , "(\\(\\(\\(\\\\)")
  , (PDFString "Hello, World!", "(Hello, World!)")
  , (PDFString "\xfe\xff\x00O\x00K", "(\xfe\xff\x00O\x00K)")
  , (PDFString "\xfe\xff\x00O\x00)\x00K", "(\xfe\xff\x00O\x00\\)\x00K)")
  , (PDFString "\xfe\xff\x00O\x00\\\x00K", "(\xfe\xff\x00O\x00\\\\\x00K)")
  , (PDFString "La Ban (donn\233es de r\233f\233rence)"
    , "(La Ban \\(donn\233es de r\233f\233rence\\))"
    )
  , (PDFString "\xfe\xff\x00\x4c\x00\x61\x00\x20\x00\x42\x00\x61\x00\x6e\x00\
      \\x20\x00\x28\x00\x64\x00\x6f\x00\x6e\x00\x6e\x00\xe9\x00\x65\x00\x73\x00\
      \\x20\x00\x64\x00\x65\x00\x20\x00\x72\x00\xe9\x00\x66\x00\xe9\x00\x72\x00\
      \\x65\x00\x6e\x00\x63\x00\x65\x00\x29"
  , "(\xfe\xff\x00\x4c\x00\x61\x00\x20\x00\x42\x00\x61\x00\x6e\x00\x20\x00\x5c\
      \\x28\x00\x64\x00\x6f\x00\x6e\x00\x6e\x00\xe9\x00\x65\x00\x73\x00\x20\x00\
      \\x64\x00\x65\x00\x20\x00\x72\x00\xe9\x00\x66\x00\xe9\x00\x72\x00\x65\x00\
      \\x6e\x00\x63\x00\x65\x00\x5c\x29)"
    )
  ]

optimizeStringExamples :: [(PDFObject, PDFObject)]
optimizeStringExamples =
  [ (PDFHexString "48656C6C6F2C20576F726C6421", PDFString "Hello, World!")
  , ( PDFHexString "FEFF00480065006C006C006F002C00200057006F0072006C00640021"
    , PDFString "Hello, World!"
    )
  , ( PDFHexString "FEFF0046007200E9006400E9007200690063"
    , PDFString "Fr\233d\233ric"
    )
  , (PDFString "(((\\", PDFString "(((\\")
  , (PDFHexString "FEFF2192", PDFString "\xfe\xff\x21\x92")
  , (PDFHexString "feff005", PDFString "P")
  , (PDFString "\xfe\xff\x00\x4c\x00\x61\x00\x20\x00\x42\x00\x61\x00\x6e\x00\
      \\x20\x00\x28\x00\x64\x00\x6f\x00\x6e\x00\x6e\x00\xe9\x00\x65\x00\x73\x00\
      \\x20\x00\x64\x00\x65\x00\x20\x00\x72\x00\xe9\x00\x66\x00\xe9\x00\x72\x00\
      \\x65\x00\x6e\x00\x63\x00\x65\x00\x29"
    , PDFString "La Ban (donn\233es de r\233f\233rence)"
    )
  , ( PDFString "La Ban (donn\233es de r\233f\233rence)"
    , PDFString "La Ban (donn\233es de r\233f\233rence)"
   )
  ]

spec :: Spec
spec = describe "PDFString" $ do
  forM_ stringExamples $ \(example, expected) ->
    it ("should convert to bytestring " ++ show example)
      $          fromPDFObject example
      `shouldBe` expected

  forM_ optimizeStringExamples $ \(example, expected) ->
    it ("should convert to PDFString " ++ show example) $ do
      result <- runExceptT (optimizeString example)
      result `shouldBe` Right expected
