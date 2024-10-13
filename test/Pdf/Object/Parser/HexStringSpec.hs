module PDF.Object.Parser.HexStringSpec
  ( spec
  ) where

import Data.ByteString qualified as BS

import PDF.Object.Object (PDFObject (PDFHexString))
import PDF.Object.Parser.HexString (hexStringP)

import Test.Hspec (Spec, describe)

import Util.ParserHelper (itWith)


hexStringExamples :: [(BS.ByteString, PDFObject)]
hexStringExamples =
  [ ("<>"                , PDFHexString "")
  , ("<   >"             , PDFHexString "")
  , ("< 1 2 3 >"         , PDFHexString "123")
  , ("<a bc de f>"       , PDFHexString "abcdef")
  , ("<A BC DE F>"       , PDFHexString "abcdef")
  ]

spec :: Spec
spec = describe "hexStringP" $ do
  mapM_ (itWith "should work with " hexStringP) hexStringExamples
