module Data.PDF.GraphicsStateSpec
  ( spec
  ) where

import Control.Monad (forM_)

import Data.PDF.GraphicsState
    ( GraphicsState (gsCTM, gsScaleX, gsScaleY, gsTextState, gsUserUnit)
    , defaultGraphicsState
    , usefulGraphicsPrecision
    )
import Data.PDF.TextState (defaultTextState)
import Data.PDF.TransformationMatrix
    ( TransformationMatrix (TransformationMatrix)
    )

import Test.Hspec (Spec, describe, it, shouldBe)

stateExamples :: [(GraphicsState, Int)]
stateExamples =
  [ ( defaultGraphicsState
      { gsUserUnit = 1.0
      , gsCTM = TransformationMatrix 1.0 0.0 0.0 1.0 0.0 0.0
      , gsTextState = defaultTextState
      , gsScaleX = 1.0
      , gsScaleY = 1.0
      }
    , 2
    )
  , ( defaultGraphicsState
      { gsUserUnit = 1.0
      , gsCTM = TransformationMatrix 0.0 1.0 (-1.0) 0.0 1.0 1.0
      , gsTextState = defaultTextState
      , gsScaleX = 1.0
      , gsScaleY = 1.0
      }
    , 2
    )
  , ( defaultGraphicsState
      { gsUserUnit = 1.0
      , gsCTM = TransformationMatrix 100.0 0.0 0.0 100.0 0.0 0.0
      , gsTextState = defaultTextState
      , gsScaleX = 100.0
      , gsScaleY = 100.0
      }
    , 4
    )
  , ( defaultGraphicsState
      { gsUserUnit = 1.0
      , gsCTM = TransformationMatrix 10.0 0.0 0.0 10.0 0.0 0.0
      , gsTextState = defaultTextState
      , gsScaleX = 10.0
      , gsScaleY = 10.0
      }
    , 3
    )
  ]

spec :: Spec
spec =
  describe "usefuleGraphicsPrecision" $
    forM_ stateExamples $ \(example, expected) -> do
      it ("should work with " ++ show example)
        $ usefulGraphicsPrecision example `shouldBe` expected
