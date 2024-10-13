module PDF.Graphics.Parser.NameSpec
  ( spec
  ) where

import Data.Binary.Parser (parseDetail)
import Data.ByteString (ByteString)
import Data.PDF.GFXObject (GFXObject (GFXName))

import PDF.Graphics.Parser.Name (nameP)

import Test.Hspec (Spec, describe, it)

import Util.ParserHelper (itWith, shouldBeFullyParsed, shouldBeParsedAs)

nameExamples :: [(ByteString, GFXObject)]
nameExamples =
  [ ("/Name1"              , GFXName "Name1")
  , ("/ASomewhatLongerName", GFXName "ASomewhatLongerName")
  , ( "/A;Name_With-Various***Characters?"
    , GFXName "A;Name_With-Various***Characters?"
    )
  , ("/1.2"                    , GFXName "1.2")
  , ("/$$"                     , GFXName "$$")
  , ("/@pattern"               , GFXName "@pattern")
  , ("/.notdef"                , GFXName ".notdef")
  , ("/Lime#20Green"           , GFXName "Lime Green")
  , ("/paired#28#29parentheses", GFXName "paired()parentheses")
  , ("/The_Key_of_F#23_Minor"  , GFXName "The_Key_of_F#_Minor")
  , ("/A#42"                   , GFXName "AB")
  , ("/A#4d"                   , GFXName "AM")
  , ("/A#4D"                   , GFXName "AM")
  ]

spec :: Spec
spec = describe "nameP" $ do
  it "should work with simple name /a" $ do
    let parsed = parseDetail nameP "/a"
    shouldBeFullyParsed parsed
    parsed `shouldBeParsedAs` GFXName "a"

  mapM_ (itWith "should work with specifications examples " nameP) nameExamples
