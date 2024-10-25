module PDF.Object.Parser.LooseEndOfLineSpec
  ( spec
  ) where

import Data.Binary.Parser (sepBy, some', takeTill)
import Data.ByteString (ByteString)

import PDF.Object.Parser.LooseEndOfLine (isLooseEndOfLine, looseEndOfLineP)

import Test.Hspec (Spec, describe)

import Util.ParserHelper (itWith)

leolExamples :: [(ByteString, [ByteString])]
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
