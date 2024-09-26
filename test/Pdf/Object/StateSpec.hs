module Pdf.Object.StateSpec
  ( spec
  ) where

import Control.Monad (forM_)
import Control.Monad.Trans.Except (runExceptT)

import Data.Fallible (FallibleT)
import Data.Logging (Logging)
import Data.Map.Strict qualified as Map

import Pdf.Object.Object
    ( PDFObject (PDFDictionary, PDFEndOfFile, PDFIndirectObject, PDFIndirectObjectWithStream, PDFName, PDFNull, PDFNumber, PDFObjectStream)
    , mkEmptyPDFArray
    , mkEmptyPDFDictionary
    , mkPDFArray
    , mkPDFDictionary
    )
import Pdf.Object.State (embedObject, setMaybe, setStream)

import Test.Hspec (Spec, describe, it, shouldBe)

import Util.Dictionary (mkDictionary, mkEmptyDictionary)

setMaybeExamples
  :: Logging m => [(PDFObject, PDFObject -> FallibleT m PDFObject, PDFObject)]
setMaybeExamples =
  [ (mkEmptyPDFArray, setMaybe "Num" (Just $ PDFNumber 1.0), mkEmptyPDFArray)
  , (mkEmptyPDFArray, setMaybe "Num" Nothing               , mkEmptyPDFArray)
  , (PDFEndOfFile   , setMaybe "Num" (Just $ PDFNumber 1.0), PDFEndOfFile)
  , ( mkEmptyPDFDictionary
    , setMaybe "Num" (Just $ PDFNumber 1.0)
    , mkPDFDictionary [("Num", PDFNumber 1.0)]
    )
  , (mkEmptyPDFDictionary, setMaybe "Num" Nothing, PDFDictionary Map.empty)
  , ( PDFDictionary (Map.fromList [("Num", PDFNull)])
    , setMaybe "Num" (Just $ PDFNumber 1.0)
    , PDFDictionary (Map.fromList [("Num", PDFNumber 1.0)])
    )
  , ( PDFDictionary (Map.fromList [("Num", PDFNull)])
    , setMaybe "Num" Nothing
    , PDFDictionary (Map.fromList [("Num", PDFNull)])
    )
  ]

setStreamExamples
  :: Logging m => [(PDFObject, PDFObject -> FallibleT m PDFObject, PDFObject)]
setStreamExamples =
  [ (mkEmptyPDFArray     , setStream "abc", mkEmptyPDFArray)
  , (PDFEndOfFile        , setStream "abc", PDFEndOfFile)
  , (mkEmptyPDFDictionary, setStream "abc", mkEmptyPDFDictionary)
  , ( PDFIndirectObjectWithStream 3 4 mkEmptyDictionary ""
    , setStream "abc"
    , PDFIndirectObjectWithStream 3
                                  4
                                  (mkDictionary [("Length", PDFNumber 3.0)])
                                  "abc"
    )
  , ( PDFObjectStream 7 8 (Map.fromList [("Length", PDFNumber 1.0)]) ""
    , setStream "abcd"
    , PDFObjectStream 7 8 (Map.fromList [("Length", PDFNumber 4.0)]) "abcd"
    )
  ]

embedObjectExamples
  :: Logging m => [(PDFObject, PDFObject -> FallibleT m PDFObject, PDFObject)]
embedObjectExamples =
  [ ( mkEmptyPDFArray
    , embedObject (mkPDFArray [PDFName "a"])
    , mkPDFArray [PDFName "a"]
    )
  , ( PDFIndirectObject 1 0 PDFNull
    , embedObject (mkPDFArray [PDFName "a"])
    , PDFIndirectObject 1 0 (mkPDFArray [PDFName "a"])
    )
  , ( PDFObjectStream 8 5 mkEmptyDictionary ""
    , embedObject (mkPDFDictionary [("a", PDFNull)])
    , PDFObjectStream 8 5 (mkDictionary [("a", PDFNull)]) ""
    )
  ]

spec :: Spec
spec = do
  describe "setMaybe" $ forM_ setMaybeExamples $ \(example, fn, expected) ->
    it ("should work on " ++ show example) $ do
      result <- runExceptT (fn example)
      result `shouldBe` Right expected

  describe "setStream" $ forM_ setStreamExamples $ \(example, fn, expected) ->
    it ("should work on " ++ show example) $ do
      result <- runExceptT (fn example)
      result `shouldBe` Right expected

  describe "embedObject"
    $ forM_ embedObjectExamples
    $ \(example, fn, expected) -> it ("should work on " ++ show example) $ do
        result <- runExceptT (fn example)
        result `shouldBe` Right expected
