module PDF.Graphics.Interpreter.OptimizeParametersSpec
  ( spec
  ) where

import Control.Monad (forM_)

import Data.PDF.GFXObject
  (GFXObject (GFXHexString, GFXName, GFXString), mkGFXArray, mkGFXDictionary)

import PDF.Graphics.Interpreter.OptimizeParameters (convertHexString)

import Test.Hspec (Spec, describe, it, shouldBe)

convertHexStringExamples :: [(GFXObject, GFXObject)]
convertHexStringExamples =
  [ ( GFXString "Hello, World!"
    , GFXString "Hello, World!"
    )
  , ( GFXHexString "48656c6c6f2c20576f726c6421"
    , GFXString "Hello, World!"
    )
  , ( mkGFXArray [GFXHexString "48656c6c6f2c20576f726c6421"]
    , mkGFXArray [GFXString "Hello, World!"]
    )
  , ( mkGFXArray
        [ GFXName "Span"
        , mkGFXDictionary [("ActualText", GFXHexString "48656c6c6f2c20576f726c6421")]
        ]
    , mkGFXArray
        [ GFXName "Span"
        , mkGFXDictionary [("ActualText", GFXString "Hello, World!")]
        ]
    )
  , ( mkGFXDictionary
        [ ( "A"
          , mkGFXDictionary
              [ ( "B"
                , GFXHexString "48656c6c6f2c20576f726c6421"
                )
              ]
          )
        ]
    , mkGFXDictionary
        [ ( "A"
          , mkGFXDictionary
              [ ( "B"
                , GFXString "Hello, World!"
                )
              ]
          )
        ]
    )
  ]

spec :: Spec
spec = do
  describe "convertHexString" $
    forM_ convertHexStringExamples $ \(example, expected) -> do
      it ("should work with " ++ show example)
        $ convertHexString example `shouldBe` expected
