module Data.PDF.ProgramSpec
  ( spec
  ) where

import Control.Monad (forM_)

import Data.PDF.Command (mkCommand)
import Data.PDF.GFXObject
    ( GFXObject (GFXNumber, GFXOperator)
    , GSOperator (GSRestoreGS, GSSaveGS, GSSetCTM)
    )
import Data.PDF.GFXObjects (GFXObjects)
import Data.PDF.Program (Program, mkProgram, parseProgram)
import Data.Sequence qualified as SQ

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
