module PDF.Graphics.Interpreter.OptimizeProgram.OptimizeMarkedContentSpec
  ( spec
  ) where

import Control.Monad (forM_)

import Data.PDF.Command (mkCommand)
import Data.PDF.GFXObject
  ( GFXObject (GFXNumber)
  , GSOperator (GSBeginMarkedContentSequencePL, GSEndMarkedContentSequence, GSSetCTM)
  )
import Data.PDF.Program (Program, mkProgram)

import PDF.Graphics.Interpreter.OptimizeProgram.OptimizeMarkedContent
  (optimizeMarkedContent)

import Test.Hspec (Spec, describe, it, shouldBe)


optimizeMarkedContentExamples :: [(Int, Program, Program)]
optimizeMarkedContentExamples =
  [ (0, mempty, mempty)
  , ( 1
    , mkProgram [mkCommand GSBeginMarkedContentSequencePL []]
    , mkProgram [mkCommand GSBeginMarkedContentSequencePL []]
    )
  , ( 2
    , mkProgram
        [ mkCommand GSBeginMarkedContentSequencePL []
        , mkCommand GSBeginMarkedContentSequencePL []
        ]
    , mkProgram
        [ mkCommand GSBeginMarkedContentSequencePL []
        , mkCommand GSBeginMarkedContentSequencePL []
        ]
    )
  , ( 3
    , mkProgram
        [ mkCommand GSBeginMarkedContentSequencePL []
        , mkCommand GSBeginMarkedContentSequencePL []
        , mkCommand GSEndMarkedContentSequence []
        ]
    , mkProgram
        [ mkCommand GSBeginMarkedContentSequencePL []
        , mkCommand GSBeginMarkedContentSequencePL []
        , mkCommand GSEndMarkedContentSequence []
        ]
    )
  , ( 4
    , mkProgram
        [ mkCommand GSBeginMarkedContentSequencePL []
        , mkCommand GSBeginMarkedContentSequencePL []
        , mkCommand GSEndMarkedContentSequence []
        , mkCommand GSEndMarkedContentSequence []
        ]
    , mkProgram
        [ mkCommand GSBeginMarkedContentSequencePL []
        , mkCommand GSEndMarkedContentSequence []
        ]
    )
  , ( 5
    , mkProgram
        [ mkCommand GSBeginMarkedContentSequencePL []
        , mkCommand GSBeginMarkedContentSequencePL [GFXNumber 2]
        , mkCommand GSEndMarkedContentSequence []
        , mkCommand GSEndMarkedContentSequence []
        ]
    , mkProgram
        [ mkCommand GSBeginMarkedContentSequencePL []
        , mkCommand GSBeginMarkedContentSequencePL [GFXNumber 2]
        , mkCommand GSEndMarkedContentSequence []
        , mkCommand GSEndMarkedContentSequence []
        ]
    )
  , ( 6
    , mkProgram
        [ mkCommand GSBeginMarkedContentSequencePL []
        , mkCommand GSBeginMarkedContentSequencePL []
        , mkCommand GSSetCTM []
        , mkCommand GSEndMarkedContentSequence []
        , mkCommand GSEndMarkedContentSequence []
        ]
    , mkProgram
        [ mkCommand GSBeginMarkedContentSequencePL []
        , mkCommand GSSetCTM []
        , mkCommand GSEndMarkedContentSequence []
        ]
    )
  , ( 7
    , mkProgram
        [ mkCommand GSBeginMarkedContentSequencePL []
        , mkCommand GSBeginMarkedContentSequencePL [GFXNumber 4]
        , mkCommand GSSetCTM []
        , mkCommand GSEndMarkedContentSequence []
        , mkCommand GSEndMarkedContentSequence []
        ]
    , mkProgram
        [ mkCommand GSBeginMarkedContentSequencePL []
        , mkCommand GSBeginMarkedContentSequencePL [GFXNumber 4]
        , mkCommand GSSetCTM []
        , mkCommand GSEndMarkedContentSequence []
        , mkCommand GSEndMarkedContentSequence []
        ]
    )
  ]

spec :: Spec
spec = do
  describe "optimizeMarkedContent" $
    forM_ optimizeMarkedContentExamples $ \(index, example, expected) -> do
      it ("should work with example " ++ show index)
        $ optimizeMarkedContent example `shouldBe` expected
