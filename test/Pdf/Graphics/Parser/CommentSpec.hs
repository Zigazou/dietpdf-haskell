module PDF.Graphics.Parser.CommentSpec
  ( spec
  ) where

import Data.Binary.Parser (endOfInput)
import Data.ByteString (ByteString)
import Data.PDF.GFXObject (GFXObject (GFXComment))

import PDF.Graphics.Parser.Comment (commentP)

import Test.Hspec (Spec, describe)

import Util.ParserHelper (itWith)

commentExamples :: [(ByteString, GFXObject)]
commentExamples =
  [ ("%ABC\n"  , GFXComment "ABC")
  , ("%ABC\r\n", GFXComment "ABC")
  , ("%ABC\r"  , GFXComment "ABC")
  ]

spec :: Spec
spec = describe "commentP" $ mapM_
  (itWith "should work with " (commentP <* endOfInput))
  commentExamples
