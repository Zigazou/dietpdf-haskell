{-# LANGUAGE OverloadedStrings #-}
module Pdf.Graphics.Parser.CommentSpec
  ( spec
  ) where

import           Test.Hspec                     ( Spec
                                                , describe
                                                )
import qualified Data.ByteString               as BS
import           Pdf.Graphics.Object            ( GFXObject(GFXComment) )
import           Data.Binary.Parser             ( endOfInput )
import           Pdf.Graphics.Parser.Comment    ( commentP )
import           Util.ParserHelper              ( itWith )

commentExamples :: [(BS.ByteString, GFXObject)]
commentExamples =
  [ ("%ABC\n"  , GFXComment "ABC")
  , ("%ABC\r\n", GFXComment "ABC")
  , ("%ABC\r"  , GFXComment "ABC")
  ]

spec :: Spec
spec = describe "commentP" $ mapM_
  (itWith "should work with " (commentP <* endOfInput))
  commentExamples
