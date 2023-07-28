module Pdf.Object.Parser.StartXRefSpec
  ( spec
  ) where

import           Test.Hspec                     ( describe
                                                , Spec
                                                )

import qualified Data.ByteString               as BS
import           Util.ParserHelper              ( itWith )
import           Pdf.Object.Parser.StartXRef    ( startXRefP )
import           Pdf.Object.Object              ( PDFObject(PDFStartXRef) )

startXRefExamples :: [(BS.ByteString, PDFObject)]
startXRefExamples = [("startxref\n\
      \1984\n", PDFStartXRef 1984)]

spec :: Spec
spec = describe "startXRefP"
  $ mapM_ (itWith "should work with " startXRefP) startXRefExamples
