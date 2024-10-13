module PDF.Graphics.Interpreter.ProgramSpec
  ( spec
  ) where

import Control.Monad (forM_)

import Data.PDF.GFXObject
    ( GFXObject (GFXNumber, GFXOperator)
    , GSOperator (GSRestoreGS, GSSaveGS, GSSetCTM)
    )
import Data.PDF.GFXObjects (GFXObjects)
import Data.Sequence qualified as SQ

import PDF.Graphics.Interpreter.Command (mkCommand)
import PDF.Graphics.Interpreter.Program (Program, mkProgram, parseProgram)

import Test.Hspec (Spec, describe, it, shouldBe)

parseProgramExamples :: [(GFXObjects, Program)]
parseProgramExamples =
  [ ( SQ.fromList []
    , mkProgram []
    )
  , ( SQ.fromList
        [ GFXNumber 1.0
        , GFXNumber 2.0
        , GFXNumber 3.0
        , GFXOperator GSSetCTM
        ]
    , mkProgram [mkCommand GSSetCTM [GFXNumber 1.0,GFXNumber 2.0,GFXNumber 3.0]]
    )
  , ( SQ.fromList
        [ GFXOperator GSSaveGS
        , GFXOperator GSSetCTM
        , GFXOperator GSRestoreGS
        ]
    , mkProgram
        [ mkCommand GSSaveGS []
        , mkCommand GSSetCTM []
        , mkCommand GSRestoreGS []
        ]
    )
  ]

spec :: Spec
spec = do
  describe "parseProgram" $
    forM_ parseProgramExamples $ \(example, expected) -> do
      it ("should work with " ++ show example)
        $          parseProgram example
        `shouldBe` expected
