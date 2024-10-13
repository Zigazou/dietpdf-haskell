module PDF.Graphics.Parser.NumberSpec
  ( spec
  ) where

import Data.ByteString (ByteString)
import Data.PDF.GFXObject (GFXObject (GFXNumber))

import PDF.Graphics.Parser.Number (numberP)

import Test.Hspec (Spec, describe)

import Util.ParserHelper (itWith)

numberExamples :: [(ByteString, GFXObject)]
numberExamples =
  [ ("123"   , GFXNumber 123.0)
  , ("43445" , GFXNumber 43445.0)
  , ("+17"   , GFXNumber 17.0)
  , ("-98"   , GFXNumber (-98.0))
  , ("0"     , GFXNumber 0.0)
  , ("34.5"  , GFXNumber 34.5)
  , ("-3.62" , GFXNumber (-3.62))
  , ("+123.6", GFXNumber 123.6)
  , ("4."    , GFXNumber 4.0)
  , ("-.002" , GFXNumber (-0.002))
  , ("0.0"   , GFXNumber 0.0)
  ]

spec :: Spec
spec = describe "numberP" $ mapM_
  (itWith "should work with specifications examples " numberP)
  numberExamples
