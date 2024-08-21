module Pdf.Object.Parser.StartXRefSpec
  ( spec
  ) where

import Data.ByteString qualified as BS

import Pdf.Object.Object (PDFObject (PDFStartXRef))
import Pdf.Object.Parser.StartXRef (startXRefP)

import Test.Hspec (Spec, describe)

import Util.ParserHelper (itWith)

startXRefExamples :: [(BS.ByteString, PDFObject)]
startXRefExamples = [("startxref\n\
      \1984\n", PDFStartXRef 1984)]

spec :: Spec
spec = describe "startXRefP"
  $ mapM_ (itWith "should work with " startXRefP) startXRefExamples
