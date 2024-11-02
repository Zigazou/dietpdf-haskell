{-# LANGUAGE PatternSynonyms #-}
module PDF.Graphics.Interpreter.OptimizeProgram.OptimizeSaveRestoreSpec
  ( spec
  ) where

import Control.Monad (forM_)

import Data.PDF.Command (Command (Command), mkCommand)
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
import Data.Sequence (Seq(Empty))

pattern Restore :: Command
pattern Restore = Command GSRestoreGS Empty

pattern Save :: Command
pattern Save = Command GSSaveGS Empty

pattern Dummy :: Command
pattern Dummy = Command (GSUnknown "dummy") Empty

pattern SetStrokeColor :: Command
pattern SetStrokeColor = Command GSSetStrokeColor Empty

findRelatedSaveExamples :: [(Program, (Program, Program))]
findRelatedSaveExamples =
  [ ( mkProgram [ Save, Dummy ]
    , (mempty, mkProgram [ Save, Dummy ])
    )
  , ( mkProgram [ Save, Save ]
    , ( mkProgram [Save], mkProgram [Save] )
    )
  , ( mkProgram [ Save, Save, Dummy, Restore ]
    , ( mempty
      , mkProgram [ Save, Save, Dummy, Restore ]
      )
    )
  , ( mkProgram [ Save, Dummy, Save, Dummy, Restore, Save, Restore]
    , ( mempty
      , mkProgram [Save, Dummy, Save, Dummy, Restore, Save, Restore]
      )
    )
  , ( mkProgram [ Save, Dummy, Save, Save, Dummy, Restore, Restore, Save, Restore]
    , ( mempty
      , mkProgram [Save, Dummy, Save, Save, Dummy, Restore, Restore, Save, Restore]
      )
    )
  ]

optimizeSaveRestoreExamples :: [(Int, Program, Program)]
optimizeSaveRestoreExamples =
  [ (1, mempty, mempty)
  , ( 2
    , mkProgram [mkCommand GSMoveTo [GFXNumber 1.000042, GFXNumber 2.421]]
    , mkProgram [mkCommand GSMoveTo [GFXNumber 1.000042, GFXNumber 2.421]]
    )
  , ( 3
    , mkProgram [Save, SetStrokeColor, Restore]
    , mkProgram [Save, SetStrokeColor, Restore]
    )
  , ( 4
    , mkProgram [Save, Save, SetStrokeColor, Restore, Restore]
    , mkProgram [Save, SetStrokeColor, Restore]
    )
  , ( 5
    , mkProgram [Save, Save, Save, SetStrokeColor, Restore, Restore, Restore]
    , mkProgram [Save, SetStrokeColor, Restore]
    )
  , ( 6
    , mkProgram [Dummy, Save, Dummy, Save, Dummy, Restore, Restore, Dummy, Dummy]
    , mkProgram [Dummy, Save, Dummy, Dummy, Restore, Dummy, Dummy]
    )
  , ( 7
    , mkProgram [Dummy, Save, Dummy, Save, Dummy, Restore, Restore, Dummy, Save, Dummy, Restore]
    , mkProgram [Dummy, Save, Dummy, Dummy, Restore, Dummy, Save, Dummy, Restore]
    )
  , ( 8
    , mkProgram [Dummy, Save, Dummy, Save, Dummy, Restore, Restore, Save, Dummy, Save, Dummy, Restore, Restore]
    , mkProgram [Dummy, Save, Dummy, Dummy, Restore, Save, Dummy, Dummy, Restore]
    )
  , ( 9
    , mkProgram
        [ Save
        , SetStrokeColor
        , Save
        , SetStrokeColor
        , Save
        , SetStrokeColor
        , Restore
        , Restore
        , Restore
        ]
    , mkProgram [Save, SetStrokeColor, SetStrokeColor, SetStrokeColor, Restore]
    )
  , ( 10
    , mkProgram
        [ Save
        ,   SetStrokeColor
        ,   Save
        ,     SetStrokeColor
        ,     Save
        ,       SetStrokeColor
        ,     Restore
        ,   Restore
        ,   SetStrokeColor
        ,   Save
        ,     SetStrokeColor
        ,     Save
        ,       SetStrokeColor
        ,     Restore
        ,   Restore
        , Restore
        ]
    , mkProgram
        [ Save
        ,   SetStrokeColor
        ,   Save
        ,     SetStrokeColor
        ,     SetStrokeColor
        ,   Restore
        ,   SetStrokeColor
        ,   SetStrokeColor
        ,   SetStrokeColor
        , Restore
        ]
  )
  , ( 11
    , mkProgram
        [ Save
        ,   Save
        ,     SetStrokeColor
        ,   Restore
        , Restore
        , Save
        ,   Save
        ,     SetStrokeColor
        ,   Restore
        , Restore
        ]
    , mkProgram
        [ Save
        ,   SetStrokeColor
        , Restore
        , Save
        ,   SetStrokeColor
        , Restore
        ]
    )
  , ( 12
    , mkProgram
        [ Save
        ,   Save
        ,     SetStrokeColor
        ,   Restore
        , Restore
        , Save
        ,   Save
        ,     SetStrokeColor
        ,   Restore
        , Restore
        ]
    , mkProgram
        [ Save
        ,   SetStrokeColor
        , Restore
        , Save
        ,   SetStrokeColor
        , Restore
        ]
    )
  , ( 13
    , mkProgram
        [ Save
        ,   Save
        ,     Save
        ,       Dummy
        ,       Save
        ,         SetStrokeColor
        ,       Restore
        ,     Restore
        ,     Dummy
        ,   Restore
        , Restore
        , Save
        ,   Save
        ,     SetStrokeColor
        ,   Restore
        , Restore
        ]
    , mkProgram
        [ Save
        ,   Save
        ,     Dummy
        ,     SetStrokeColor
        ,   Restore
        ,   Dummy
        , Restore
        , Save
        ,   SetStrokeColor
        , Restore
        ]
    )
  , ( 14
    , mkProgram
        [ Save
        ,   Save
        ,     Save
        ,       Dummy
        ,       SetStrokeColor
        ,     Restore
        ,     Dummy
        ,   Restore
        , Restore
        , Save
        ,   SetStrokeColor
        , Restore
        ]
    , mkProgram
        [ Save
        ,   Save
        ,     Dummy
        ,     SetStrokeColor
        ,   Restore
        ,   Dummy
        , Restore
        , Save
        ,   SetStrokeColor
        , Restore
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
    forM_ optimizeSaveRestoreExamples $ \(index, example, expected) -> do
      it ("should work with example " ++ show index)
        $          optimizeSaveRestore example
        `shouldBe` expected
