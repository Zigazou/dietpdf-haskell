module Pdf.Object.Parser.CommentSpec
  ( spec
  ) where

import Data.Binary.Parser (endOfInput)
import Data.ByteString qualified as BS

import Pdf.Object.Object (PDFObject (PDFComment, PDFEndOfFile, PDFVersion))
import Pdf.Object.Parser.Comment (commentP)

import Test.Hspec (Spec, describe)

import Util.ParserHelper (itWith)

commentExamples :: [(BS.ByteString, PDFObject)]
commentExamples =
  [ ("%ABC\n"      , PDFComment "ABC")
  , ("%PDF-1.7\n"  , PDFVersion "1.7")
  , ("%%EOF\n"     , PDFEndOfFile)
  , ("%ABC\r\n"    , PDFComment "ABC")
  , ("%ABC\r"      , PDFComment "ABC")
  , ("%PDF-1.7\r\n", PDFVersion "1.7")
  , ("%PDF-1.7\r"  , PDFVersion "1.7")
  , ("%%EOF\r\n"   , PDFEndOfFile)
  , ("%%EOF\r"     , PDFEndOfFile)
  ]

spec :: Spec
spec = describe "commentP" $ mapM_
  (itWith "should work with " (commentP <* endOfInput))
  commentExamples
