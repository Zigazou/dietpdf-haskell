module PDF.Graphics.Interpreter.OptimizeProgram.OptimizeSaveRestoreSpec
  ( spec
  ) where

import Control.Monad (forM_)

import Data.PDF.Command (Command, mkCommand)
import Data.PDF.GFXObject
    ( GFXObject (GFXNumber)
    , GSOperator (GSMoveTo, GSRestoreGS, GSSaveGS, GSUnknown, GSSetStrokeColor)
    )
import Data.PDF.Program (Program, mkProgram)

import PDF.Graphics.Interpreter.OptimizeProgram.OptimizeSaveRestore
    ( findRelatedSave
    , optimizeSaveRestore
    )

import Test.Hspec (Spec, describe, it, shouldBe)


pSave, pRestore, pDummy :: Command
pSave    = mkCommand GSSaveGS []
pRestore = mkCommand GSRestoreGS []
pDummy   = mkCommand (GSUnknown "dummy") []

findRelatedSaveExamples :: [(Program, (Program, Program))]
findRelatedSaveExamples =
  [ ( mkProgram [ pSave, pDummy ]
    , (mempty, mkProgram [ pSave, pDummy ])
    )
  , ( mkProgram [ pSave, pSave ]
    , ( mkProgram [pSave], mkProgram [pSave] )
    )
  , ( mkProgram [ pSave, pSave, pDummy, pRestore ]
    , ( mempty
      , mkProgram [ pSave, pSave, pDummy, pRestore ]
      )
    )
  ]

optimizeSaveRestoreExamples :: [(Program, Program)]
optimizeSaveRestoreExamples =
  [ (mempty, mempty)
  , ( mkProgram [mkCommand GSMoveTo [GFXNumber 1.000042, GFXNumber 2.421]]
    , mkProgram [mkCommand GSMoveTo [GFXNumber 1.000042, GFXNumber 2.421]]
    )
  , ( mkProgram
        [ mkCommand GSSaveGS []
        , mkCommand GSSetStrokeColor []
        , mkCommand GSRestoreGS []
        ]
    , mkProgram
        [ mkCommand GSSaveGS []
        , mkCommand GSSetStrokeColor []
        , mkCommand GSRestoreGS []
        ]
    )
  , ( mkProgram
        [ mkCommand GSSaveGS []
        , mkCommand GSSaveGS []
        , mkCommand GSSetStrokeColor []
        , mkCommand GSRestoreGS []
        , mkCommand GSRestoreGS []
        ]
    , mkProgram
        [ mkCommand GSSaveGS []
        , mkCommand GSSetStrokeColor []
        , mkCommand GSRestoreGS []
        ]
    )
  , ( mkProgram
        [ mkCommand GSSaveGS []
        , mkCommand GSSaveGS []
        , mkCommand GSSaveGS []
        , mkCommand GSSetStrokeColor []
        , mkCommand GSRestoreGS []
        , mkCommand GSRestoreGS []
        , mkCommand GSRestoreGS []
        ]
    , mkProgram
        [ mkCommand GSSaveGS []
        , mkCommand GSSetStrokeColor []
        , mkCommand GSRestoreGS []
        ]
    )
  , ( mkProgram
        [ mkCommand GSSaveGS []
        , mkCommand GSSetStrokeColor []
        , mkCommand GSSaveGS []
        , mkCommand GSSetStrokeColor []
        , mkCommand GSSaveGS []
        , mkCommand GSSetStrokeColor []
        , mkCommand GSRestoreGS []
        , mkCommand GSRestoreGS []
        , mkCommand GSRestoreGS []
        ]
    , mkProgram
        [ mkCommand GSSaveGS []
        , mkCommand GSSetStrokeColor []
        , mkCommand GSSetStrokeColor []
        , mkCommand GSSetStrokeColor []
        , mkCommand GSRestoreGS []
        ]
    )
  , ( mkProgram
        [ mkCommand GSSaveGS []
        ,   mkCommand GSSetStrokeColor []
        ,   mkCommand GSSaveGS []
        ,     mkCommand GSSetStrokeColor []
        ,     mkCommand GSSaveGS []
        ,       mkCommand GSSetStrokeColor []
        ,     mkCommand GSRestoreGS []
        ,   mkCommand GSRestoreGS []
        ,   mkCommand GSSetStrokeColor []
        ,   mkCommand GSSaveGS []
        ,     mkCommand GSSetStrokeColor []
        ,     mkCommand GSSaveGS []
        ,       mkCommand GSSetStrokeColor []
        ,     mkCommand GSRestoreGS []
        ,   mkCommand GSRestoreGS []
        , mkCommand GSRestoreGS []
        ]
    , mkProgram
        [ mkCommand GSSaveGS []
        ,   mkCommand GSSetStrokeColor []
        ,   mkCommand GSSaveGS []
        ,     mkCommand GSSetStrokeColor []
        ,     mkCommand GSSaveGS []
        ,       mkCommand GSSetStrokeColor []
        ,     mkCommand GSRestoreGS []
        ,   mkCommand GSRestoreGS []
        ,   mkCommand GSSetStrokeColor []
        ,   mkCommand GSSetStrokeColor []
        ,   mkCommand GSSetStrokeColor []
        , mkCommand GSRestoreGS []
        ]
    )
  ]

spec :: Spec
spec = do
  describe "findRelatedSave" $
    forM_ findRelatedSaveExamples $ \(example, expected) -> do
      it ("should work with " ++ show example)
        $          findRelatedSave example
        `shouldBe` Just expected

  describe "optimizeSaveRestore" $
    forM_ optimizeSaveRestoreExamples $ \(example, expected) -> do
      it ("should work with " ++ show example)
        $          optimizeSaveRestore example
        `shouldBe` expected
