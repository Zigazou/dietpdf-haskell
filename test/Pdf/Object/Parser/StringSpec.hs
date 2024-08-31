module Pdf.Object.Parser.StringSpec
  ( spec
  ) where

import Data.Binary.Parser (parseDetail)
import Data.ByteString qualified as BS

import Pdf.Object.Object (PDFObject (PDFString))
import Pdf.Object.Parser.String (stringP)

import Test.Hspec (Spec, describe, it)

import Util.ParserHelper (itWith, shouldFail)


stringExamples :: [(BS.ByteString, PDFObject)]
stringExamples =
  [ ("()"                , PDFString "")
  , ("(abc)"             , PDFString "abc")
  , ("(\\061\\62\\063)"  , PDFString "123")
  , ("(a\\((b)\\)c)"     , PDFString "a((b))c")
  , ("(a\\(b\\)c)"       , PDFString "a(b)c")
  , ("(\\\n\\\r\n)"      , PDFString "")
  , ("(\\n\\r\\n)"       , PDFString "\n\r\n")
  , ("(()()())"          , PDFString "()()()")
  , ("((\\\n)\\\n()())"  , PDFString "()()()")
  , ("((\\\r\n)\\\n()())", PDFString "()()()")
  , ( "(\xf6\x2e\x95\x40\x01\x93\x12\xf9\xc8\xae\x5c\x29\xca\xeb\x05\x48\
      \\x81\xc1\x39\x68\xe4\x48\x2c\xc4\x9d\x91\x71\xac\xe9\xb0\x59\x5c\
      \\x72\x52)"
    , PDFString
      "\xf6\x2e\x95\x40\x01\x93\x12\xf9\xc8\xae\x29\xca\xeb\x05\x48\
      \\x81\xc1\x39\x68\xe4\x48\x2c\xc4\x9d\x91\x71\xac\xe9\xb0\x59\
      \\x0d\x52"
    )
  , ( "(\xfe\xff\x00\x46\x00\x72\x00\xe9\x00\x64\x00\xe9\x00\x72\x00\x69\x00\x63)"
    , PDFString "\xfe\xff\x00\x46\x00\x72\x00\xe9\x00\x64\x00\xe9\x00\x72\x00\x69\x00\x63"
    )
  , ( "(\xfe\xff\x00\x46\x00\x72\x00\xe9\x00\x64\x00\xe9\x00\x72\x00\x69\x00\x63\x00\\))"
    , PDFString "\xfe\xff\x00\x46\x00\x72\x00\xe9\x00\x64\x00\xe9\x00\x72\x00\x69\x00\x63\x00)"
    )
  ]

spec :: Spec
spec = describe "stringP" $ do
  it "should fail with unknown escaped character (ab\\c)" $ do
    let parsed = parseDetail stringP "(ab\\c)"
    shouldFail parsed

  mapM_ (itWith "should work with " stringP) stringExamples
