module Pdf.Graphics.Interpreter.GraphicsStateSpec
  ( spec
  ) where

import Control.Monad (forM_)

import Pdf.Graphics.Interpreter.GraphicsState
    ( GraphicsState (GraphicsState, gsCTM, gsScaleX, gsScaleY, gsStack, gsTextState, gsUserUnit)
    , usefulGraphicsPrecision
    )
import Pdf.Graphics.Interpreter.TextState (defaultTextState)
import Pdf.Graphics.Interpreter.TransformationMatrix
    ( TransformationMatrix (TransformationMatrix)
    )

import Test.Hspec (Spec, describe, it, shouldBe)

stateExamples :: [(GraphicsState, Int)]
stateExamples =
  [ ( GraphicsState
      { gsUserUnit = 1.0
      , gsCTM = TransformationMatrix 1.0 0.0 0.0 1.0 0.0 0.0
      , gsTextState = defaultTextState
      , gsStack = []
      , gsScaleX = 1.0
      , gsScaleY = 1.0
      }
    , 2
    )
  , ( GraphicsState
      { gsUserUnit = 1.0
      , gsCTM = TransformationMatrix 0.0 1.0 (-1.0) 0.0 1.0 1.0
      , gsTextState = defaultTextState
      , gsStack = []
      , gsScaleX = 1.0
      , gsScaleY = 1.0
      }
    , 2
    )
  , ( GraphicsState
      { gsUserUnit = 1.0
      , gsCTM = TransformationMatrix 100.0 0.0 0.0 100.0 0.0 0.0
      , gsTextState = defaultTextState
      , gsStack = []
      , gsScaleX = 100.0
      , gsScaleY = 100.0
      }
    , 4
    )
  , ( GraphicsState
      { gsUserUnit = 1.0
      , gsCTM = TransformationMatrix 10.0 0.0 0.0 10.0 0.0 0.0
      , gsTextState = defaultTextState
      , gsStack = []
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