{-# LANGUAGE OverloadedStrings #-}
module Pdf.Object.Parser.LooseEndOfLineSpec
  ( spec
  ) where

import           Test.Hspec                     ( Spec
                                                , describe
                                                )
import qualified Data.ByteString               as BS
import           Pdf.Object.Parser.LooseEndOfLine
                                                ( looseEndOfLineP
                                                , isLooseEndOfLine
                                                )
import           Data.Binary.Parser             ( sepBy
                                                , some'
                                                , takeTill
                                                )
import           Util.ParserHelper              ( itWith )

leolExamples :: [(BS.ByteString, [BS.ByteString])]
leolExamples =
  [ ("a\nb"    , ["a", "b"])
  , ("a\rb"    , ["a", "b"])
  , ("a\r\nb"  , ["a", "b"])
  , ("a\r\n\rb", ["a", "b"])
  ]

spec :: Spec
spec = describe "looseEndOfLineP" $ mapM_
  (itWith "should work with "
          (takeTill isLooseEndOfLine `sepBy` some' looseEndOfLineP)
  )
  leolExamples
