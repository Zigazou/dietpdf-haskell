module PDF.Processing.PDFWorkSpec
  ( spec
  ) where

import Control.Monad (forM_)
import Control.Monad.State (evalStateT)
import Control.Monad.Trans.Except (runExceptT)

import Data.ByteString (ByteString)
import Data.Fallible (FallibleT)
import Data.PDF.PDFObject
  ( PDFObject (PDFIndirectObject, PDFNull, PDFNumber)
  , mkPDFArray
  , mkPDFDictionary
  )
import Data.PDF.WorkData (WorkData, emptyWorkData)

import PDF.Processing.PDFWork (deepMapKeysP, deepMapP)

import Test.Hspec (Spec, describe, it, shouldBe)

-- Helper to run PDFWork tests in Identity monad with empty state
runTestWork :: WorkData -> FallibleT IO a -> IO (Either String a)
runTestWork = const ((either (Left . show) Right <$>) . runExceptT)

-- Test examples for deepMapP
-- Format: (input object, transformation description, expected output)
deepMapPExamples :: [(PDFObject, String, PDFObject)]
deepMapPExamples =
  [ -- Simple null object - no transformation
    ( PDFNull
    , "null unchanged"
    , PDFNull
    )
    -- Single number - gets incremented
  , ( PDFNumber 1.0
    , "number incremented"
    , PDFNumber 2.0
    )
    -- Array with numbers
  , ( mkPDFArray [PDFNumber 1.0, PDFNumber 2.0, PDFNumber 3.0]
    , "array of numbers incremented"
    , mkPDFArray [PDFNumber 2.0, PDFNumber 3.0, PDFNumber 4.0]
    )
    -- Nested array
  , ( mkPDFArray [PDFNumber 1.0, mkPDFArray [PDFNumber 2.0, PDFNumber 3.0]]
    , "nested array of numbers incremented"
    , mkPDFArray [PDFNumber 2.0, mkPDFArray [PDFNumber 3.0, PDFNumber 4.0]]
    )
    -- Dictionary with numbers
  , ( mkPDFDictionary [("a", PDFNumber 1.0), ("b", PDFNumber 2.0)]
    , "dictionary of numbers incremented"
    , mkPDFDictionary [("a", PDFNumber 2.0), ("b", PDFNumber 3.0)]
    )
    -- Nested dictionary
  , ( mkPDFDictionary
        [ ("outer", PDFNumber 1.0)
        , ("inner", mkPDFDictionary [("a", PDFNumber 2.0)])
        ]
    , "nested dictionary of numbers incremented"
    , mkPDFDictionary
        [ ("outer", PDFNumber 2.0)
        , ("inner", mkPDFDictionary [("a", PDFNumber 3.0)])
        ]
    )
    -- Indirect object containing a number
  , ( PDFIndirectObject 1 0 (PDFNumber 5.0)
    , "indirect object with number incremented"
    , PDFIndirectObject 1 0 (PDFNumber 6.0)
    )
    -- Indirect object containing a dictionary
  , ( PDFIndirectObject
        1
        0
        (mkPDFDictionary [("x", PDFNumber 10.0), ("y", PDFNumber 20.0)])
    , "indirect object with dictionary incremented"
    , PDFIndirectObject
        1
        0
        (mkPDFDictionary [("x", PDFNumber 11.0), ("y", PDFNumber 21.0)])
    )
    -- Mixed structure: array with dictionary containing nested array
  , ( mkPDFArray
        [ PDFNull
        , mkPDFDictionary [("nums", mkPDFArray [PDFNumber 1.0, PDFNumber 2.0])]
        , PDFNumber 3.0
        ]
    , "complex mixed structure incremented"
    , mkPDFArray
        [ PDFNull
        , mkPDFDictionary [("nums", mkPDFArray [PDFNumber 2.0, PDFNumber 3.0])]
        , PDFNumber 4.0
        ]
    )
  ]

-- Test examples for deepMapKeysP
-- Format: (input object, description, expected collected keys)
deepMapKeysPExamples :: [(String, PDFObject, PDFObject)]
deepMapKeysPExamples =
  [ ( "no dictionary"
    , PDFNumber 1.0
    , PDFNumber 1.0
    )

  , ( "single key dictionary"
    , mkPDFDictionary [("modify", PDFNumber 1.0)]
    , mkPDFDictionary [("modify", PDFNumber 100.0)]
    )

  , ( "multi-key dictionary"
    , mkPDFDictionary [("keep", PDFNumber 1.0), ("modify", PDFNumber 1.0)]
    , mkPDFDictionary [("keep", PDFNumber 1.0), ("modify", PDFNumber 100.0)]
    )

  , ( "nested dictionaries"
    , mkPDFDictionary [("keep", mkPDFDictionary [("modify", PDFNumber 1.0)])]
    , mkPDFDictionary [("keep", mkPDFDictionary [("modify", PDFNumber 100.0)])]
    )

  , ( "dictionary with array"
    , mkPDFDictionary [("modify", mkPDFArray [PDFNumber 1.0, PDFNumber 2.0])]
    , mkPDFDictionary [("modify", mkPDFArray [PDFNumber 100.0, PDFNumber 200.0])]
    )

  , ( "complex nested structure with no modifications"
    , mkPDFDictionary
        [ ( "keep"
          , mkPDFDictionary
            [ ( "modify"
              , mkPDFDictionary
                [ ( "keep"
                  , PDFNumber 1.0
                  )
                ]
              )
            ]
          )
        ]
    , mkPDFDictionary
        [ ( "keep"
          , mkPDFDictionary
            [ ( "modify"
              , mkPDFDictionary
                [ ( "keep"
                  , PDFNumber 1.0
                  )
                ]
              )
            ]
          )
        ]
    )

  , ( "complex nested structure with modifications"
    , mkPDFDictionary
        [ ( "keep"
          , mkPDFDictionary
            [ ( "keep"
              , mkPDFDictionary
                [ ( "modify"
                  , PDFNumber 1.0
                  )
                ]
              )
            ]
          )
        ]
    , mkPDFDictionary
        [ ( "keep"
          , mkPDFDictionary
            [ ( "keep"
              , mkPDFDictionary
                [ ( "modify"
                  , PDFNumber 100.0
                  )
                ]
              )
            ]
          )
        ]
    )

  , ( "array at root"
    , mkPDFArray [PDFNumber 1.0, PDFNumber 2.0]
    , mkPDFArray [PDFNumber 1.0, PDFNumber 2.0]
    )

  , ( "indirect object with dictionary"
    , PDFIndirectObject 1 0 (mkPDFDictionary [("modify", PDFNumber 1.0)])
    , PDFIndirectObject 1 0 (mkPDFDictionary [("modify", PDFNumber 100.0)])
    )

  , ( "multiple branches"
    , mkPDFDictionary
        [ ("keep1", mkPDFDictionary [("modify", PDFNumber 1.0)])
        , ("keep2", mkPDFDictionary [("keep", PDFNumber 2.0)])
        ]
    , mkPDFDictionary
        [ ("keep1", mkPDFDictionary [("modify", PDFNumber 100.0)])
        , ("keep2", mkPDFDictionary [("keep", PDFNumber 2.0)])
        ]
    )
  ]

-- Transformation function for deepMapP tests: increment all numbers
incrementNumbers :: Monad m => PDFObject -> m PDFObject
incrementNumbers (PDFNumber n) = return $ PDFNumber (n + 1.0)
incrementNumbers other         = return other

-- Transformation function for deepMapKeysP tests: multiply numbers under
-- "modify" key by 100
multiplyNumbers :: Monad m => [ByteString] -> PDFObject -> m PDFObject
multiplyNumbers ("modify":_remains) (PDFNumber n) = return (PDFNumber (n * 100.0))
multiplyNumbers _anyContext object = return object

spec :: Spec
spec = do
  describe "deepMapP" $ do
    forM_ deepMapPExamples $ \(input, description, expected) ->
      it ("should handle " ++ description) $ do
        result <- runTestWork emptyWorkData $ evalStateT (deepMapP incrementNumbers input) emptyWorkData
        result `shouldBe` Right expected

  describe "deepMapKeysP" $ do
    forM_ deepMapKeysPExamples $ \(description, input, expected) ->
      it ("should work for " ++ description) $ do
        result <- runTestWork emptyWorkData $
          evalStateT (deepMapKeysP [] multiplyNumbers input) emptyWorkData

        case result of
          Right _ -> return ()
          Left e  -> fail $ "Transformation failed: " ++ show e

        result `shouldBe` Right expected
