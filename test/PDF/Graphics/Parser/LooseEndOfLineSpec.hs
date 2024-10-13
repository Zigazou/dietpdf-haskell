module PDF.Graphics.Parser.LooseEndOfLineSpec
  ( spec
  ) where

import Data.Binary.Parser (sepBy, some', takeTill)
import Data.ByteString qualified as BS

import PDF.Graphics.Parser.LooseEndOfLine (isLooseEndOfLine, looseEndOfLineP)

import Test.Hspec (Spec, describe)

import Util.ParserHelper (itWith)

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
