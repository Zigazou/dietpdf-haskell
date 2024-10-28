module PDF.Graphics.Interpreter.ResourcesUsedSpec
  ( spec
  ) where

import Control.Monad (forM_)

import Data.ByteString (ByteString)
import Data.PDF.Program (parseProgram)
import Data.PDF.Resource (Resource (ResExtGState, ResFont, ResXObject))
import Data.Set (Set, fromList)

import PDF.Graphics.Interpreter.ResourcesUsed (resourcesUsed)
import PDF.Graphics.Parser.Stream (gfxParse)

import Test.Hspec (Spec, describe, it, shouldBe)

resourcesUsedExamples :: [(ByteString, Set Resource)]
resourcesUsedExamples =
  [ ("", mempty)
  , ("1.000042 2.421 m", mempty)
  , ("q cm Q", mempty)
  , ( "q 0.12 0 0 0.12 0 0 cm\n\
     \q\n\
     \/234 Do\n\
     \/R7 11.04 Tf\n\
     \/234 Do\n\
     \[(A)-4.33874(B)6.53732]TJ"
    , fromList [ResFont "R7", ResXObject "234"])
  , ( "q 0.12 0 0 0.12 0 0 cm\n\
     \q\n\
     \/234 Do\n\
     \/R7 11.04 Tf\n\
     \/abc gs\n\
     \[(A)-4.33874(B)6.53732]TJ"
    , fromList [ResFont "R7", ResXObject "234", ResExtGState "abc"])
  ]

spec :: Spec
spec = do
  describe "resourcesUsed"
    $ forM_ resourcesUsedExamples
    $ \(example, expected) -> do
        it ("should work with " ++ show example)
          $ resourcesUsed . parseProgram
              <$> gfxParse example `shouldBe` Right expected
