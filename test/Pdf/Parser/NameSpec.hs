{-# LANGUAGE OverloadedStrings #-}
module Pdf.Parser.NameSpec
  ( spec
  )
where

import           Test.Hspec                     ( describe
                                                , it
                                                , Spec
                                                )
import           Data.Binary.Parser             ( parseDetail )
import qualified Data.ByteString               as BS
import           Util.ParserHelper              ( shouldBeParsedAs
                                                , shouldBeFullyParsed
                                                , itWith
                                                )
import           Pdf.Parser.Name                ( nameP )
import           Pdf.Object.Object              ( PDFObject(PDFName) )

nameExamples :: [(BS.ByteString, PDFObject)]
nameExamples =
  [ ("/Name1"              , PDFName "Name1")
  , ("/ASomewhatLongerName", PDFName "ASomewhatLongerName")
  , ( "/A;Name_With-Various***Characters?"
    , PDFName "A;Name_With-Various***Characters?"
    )
  , ("/1.2"                    , PDFName "1.2")
  , ("/$$"                     , PDFName "$$")
  , ("/@pattern"               , PDFName "@pattern")
  , ("/.notdef"                , PDFName ".notdef")
  , ("/Lime#20Green"           , PDFName "Lime Green")
  , ("/paired#28#29parentheses", PDFName "paired()parentheses")
  , ("/The_Key_of_F#23_Minor"  , PDFName "The_Key_of_F#_Minor")
  , ("/A#42"                   , PDFName "AB")
  , ("/A#4d"                   , PDFName "AM")
  , ("/A#4D"                   , PDFName "AM")
  ]

spec :: Spec
spec = describe "nameP" $ do
  it "should work with simple name /a" $ do
    let parsed = parseDetail nameP "/a"
    shouldBeFullyParsed parsed
    parsed `shouldBeParsedAs` PDFName "a"

  mapM_ (itWith "should work with specifications examples " nameP) nameExamples
