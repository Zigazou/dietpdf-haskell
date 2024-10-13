module PDF.Object.StringSpec
  ( spec
  ) where

import Control.Monad (forM_)

import Data.ByteString (ByteString)
import Data.PDF.PDFObject (PDFObject (PDFHexString, PDFString))
import Data.PDF.PDFWork (evalPDFWorkT)

import PDF.Object.Object (fromPDFObject)
import PDF.Object.String (optimizeString)

import Test.Hspec (Spec, describe, it, shouldBe)

stringExamples :: [(PDFObject, ByteString)]
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
  , (PDFHexString "FEFF004F007000740069006D00690073006100740069006F006E00A00021"
    , PDFString "\xFE\xFF\x00\x4F\x00\x70\x00\x74\x00\x69\x00\x6D\x00\x69\x00\x73\x00\x61\x00\x74\x00\x69\x00\x6F\x00\x6E\x00\xA0\x00\x21"
    )
  , (PDFHexString "FEFF004F007000740069006D00690073006100740069006F006E202F0021"
    , PDFString "\xFE\xFF\x00\x4F\x00\x70\x00\x74\x00\x69\x00\x6D\x00\x69\x00\x73\x00\x61\x00\x74\x00\x69\x00\x6F\x00\x6E\x20\x2F\x00\x21"
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
      result <- evalPDFWorkT (optimizeString example)
      result `shouldBe` Right expected
