module PDF.Object.Parser.StartXRefSpec
  ( spec
  ) where


import Data.ByteString (ByteString)

import PDF.Object.Object (PDFObject (PDFStartXRef))
import PDF.Object.Parser.StartXRef (startXRefP)

import Test.Hspec (Spec, describe)

import Util.ParserHelper (itWith)

startXRefExamples :: [(ByteString, PDFObject)]
startXRefExamples = [("startxref\n\
      \1984\n", PDFStartXRef 1984)]

spec :: Spec
spec = describe "startXRefP"
  $ mapM_ (itWith "should work with " startXRefP) startXRefExamples
