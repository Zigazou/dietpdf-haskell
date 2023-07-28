module Pdf.Graphics.Parser.NameSpec
  ( spec
  ) where

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
import           Pdf.Graphics.Parser.Name       ( nameP )
import           Pdf.Graphics.Object            ( GFXObject(GFXName) )

nameExamples :: [(BS.ByteString, GFXObject)]
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
