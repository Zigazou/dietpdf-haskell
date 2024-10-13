module PDF.Object.Parser.NumberSpec
  ( spec
  ) where

import Data.ByteString qualified as BS

import PDF.Object.Object (PDFObject (PDFNumber))
import PDF.Object.Parser.Number (numberP)

import Test.Hspec (Spec, describe)

import Util.ParserHelper (itWith)

numberExamples :: [(BS.ByteString, PDFObject)]
numberExamples =
  [ ("123"   , PDFNumber 123.0)
  , ("43445" , PDFNumber 43445.0)
  , ("+17"   , PDFNumber 17.0)
  , ("-98"   , PDFNumber (-98.0))
  , ("0"     , PDFNumber 0.0)
  , ("34.5"  , PDFNumber 34.5)
  , ("-3.62" , PDFNumber (-3.62))
  , ("+123.6", PDFNumber 123.6)
  , ("4."    , PDFNumber 4.0)
  , ("-.002" , PDFNumber (-0.002))
  , ("0.0"   , PDFNumber 0.0)
  ]

spec :: Spec
spec = describe "numberP" $ mapM_
  (itWith "should work with specifications examples " numberP)
  numberExamples
