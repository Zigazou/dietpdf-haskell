module Pdf.Graphics.Parser.StringSpec
  ( spec
  ) where

import           Test.Hspec                     ( describe
                                                , it
                                                , Spec
                                                )

import           Data.Binary.Parser             ( parseDetail )
import qualified Data.ByteString               as BS
import           Util.ParserHelper              ( shouldFail
                                                , itWith
                                                )
import           Pdf.Graphics.Parser.String     ( stringP )
import           Pdf.Graphics.Object            ( GFXObject(GFXString) )

stringExamples :: [(BS.ByteString, GFXObject)]
stringExamples =
  [ ("()"                , GFXString "")
  , ("(abc)"             , GFXString "abc")
  , ("(\\061\\62\\063)"  , GFXString "123")
  , ("(a\\((b)\\)c)"     , GFXString "a((b))c")
  , ("(a\\(b\\)c)"       , GFXString "a(b)c")
  , ("(\\\n\\\r\n)"      , GFXString "")
  , ("(\\n\\r\\n)"       , GFXString "\n\r\n")
  , ("(()()())"          , GFXString "()()()")
  , ("((\\\n)\\\n()())"  , GFXString "()()()")
  , ("((\\\r\n)\\\n()())", GFXString "()()()")
  ]

spec :: Spec
spec = describe "stringP" $ do
  it "should fail with unknown escaped character (ab\\c)" $ do
    let parsed = parseDetail stringP "(ab\\c)"
    shouldFail parsed

  mapM_ (itWith "should work with " stringP) stringExamples
