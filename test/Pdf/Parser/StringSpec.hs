{-# LANGUAGE OverloadedStrings #-}
module Pdf.Parser.StringSpec
  ( spec
  )
where

import           Test.Hspec                     ( describe
                                                , it
                                                , Spec
                                                )

import           Data.Binary.Parser             ( parseDetail )
import qualified Data.ByteString               as BS
import           Util.ParserHelper              ( shouldFail
                                                , itWith
                                                )
import           Pdf.Parser.String              ( stringP )
import           Pdf.Object.Object              ( PDFObject(PDFString) )


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
  ]

spec :: Spec
spec = describe "stringP" $ do
  it "should fail with unknown escaped character (ab\\c)" $ do
    let parsed = parseDetail stringP "(ab\\c)"
    shouldFail parsed

  mapM_ (itWith "should work with " stringP) stringExamples
