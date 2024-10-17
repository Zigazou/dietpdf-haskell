module Data.PDF.TransformationMatrixSpec
  ( spec
  ) where

import Control.Monad (forM_)

import Data.PDF.TransformationMatrix
    ( TransformationMatrix (TransformationMatrix)
    , prod
    )

import Test.Hspec (Spec, describe, it, shouldBe)

prodExamples :: [(TransformationMatrix, TransformationMatrix, TransformationMatrix)]
prodExamples =
  [ ( TransformationMatrix 0.0 10.432 (-10.432) 0.0 652.0259 540.9921
    , TransformationMatrix 1.0 0.0    0.0       1.0 0.0      0.0
    , TransformationMatrix 0.0 10.432 (-10.432) 0.0 652.0259 540.9921
    )
  , ( TransformationMatrix 1.0 0.0    0.0       1.0 0.0      0.0
    , TransformationMatrix 0.0 10.432 (-10.432) 0.0 652.0259 540.9921
    , TransformationMatrix 0.0 10.432 (-10.432) 0.0 652.0259 540.9921
    )
  ]

spec :: Spec
spec =
  describe "prod" $
    forM_ prodExamples $ \(matrix1, matrix2, expected) -> do
      it ("should work with " ++ show matrix1 ++ " and " ++ show matrix2)
        $ prod matrix1 matrix2 `shouldBe` expected
