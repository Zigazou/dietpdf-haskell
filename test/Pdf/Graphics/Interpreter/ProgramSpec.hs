module Pdf.Graphics.Interpreter.ProgramSpec
  ( spec
  ) where

import Control.Monad (forM_)

import Data.Sequence qualified as SQ

import Pdf.Graphics.Interpreter.Command (mkCommand)
import Pdf.Graphics.Interpreter.Program (Program, mkProgram, parseProgram)
import Pdf.Graphics.Object
    ( GFXObject (GFXNumber, GFXOperator)
    , GSOperator (GSRestoreGS, GSSaveGS, GSSetCTM)
    )
import Pdf.Graphics.Objects (Objects)

import Test.Hspec (Spec, describe, it, shouldBe)

parseProgramExamples :: [(Objects, Program)]
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
