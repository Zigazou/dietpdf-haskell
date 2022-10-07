{-# LANGUAGE OverloadedStrings #-}
module Pdf.Object.StateSpec
  ( spec
  ) where

import           Control.Monad                  ( forM_ )
import qualified Data.Map.Strict               as Map
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFArray
                                                  , PDFDictionary
                                                  , PDFEndOfFile
                                                  , PDFIndirectObject
                                                  , PDFIndirectObjectWithStream
                                                  , PDFName
                                                  , PDFNull
                                                  , PDFNumber
                                                  , PDFObjectStream
                                                  )
                                                )
import           Pdf.Object.State               ( FallibleComputation
                                                , ObjectComputation
                                                , embedObject
                                                , setMaybe
                                                , setStream
                                                , update
                                                , updateE
                                                )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )

setMaybeExamples :: Monad m => [(PDFObject, ObjectComputation m (), PDFObject)]
setMaybeExamples =
  [ (PDFArray [] , setMaybe "Num" (Just $ PDFNumber 1.0), PDFArray [])
  , (PDFArray [] , setMaybe "Num" Nothing               , PDFArray [])
  , (PDFEndOfFile, setMaybe "Num" (Just $ PDFNumber 1.0), PDFEndOfFile)
  , ( PDFDictionary Map.empty
    , setMaybe "Num" (Just $ PDFNumber 1.0)
    , PDFDictionary (Map.fromList [("Num", PDFNumber 1.0)])
    )
  , (PDFDictionary Map.empty, setMaybe "Num" Nothing, PDFDictionary Map.empty)
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
  :: Monad m => [(PDFObject, ObjectComputation m (), PDFObject)]
setStreamExamples =
  [ (PDFArray []            , setStream "abc", PDFArray [])
  , (PDFEndOfFile           , setStream "abc", PDFEndOfFile)
  , (PDFDictionary Map.empty, setStream "abc", PDFDictionary Map.empty)
  , ( PDFIndirectObjectWithStream 3 4 Map.empty ""
    , setStream "abc"
    , PDFIndirectObjectWithStream 3
                                  4
                                  (Map.fromList [("Length", PDFNumber 3.0)])
                                  "abc"
    )
  , ( PDFObjectStream 7 8 (Map.fromList [("Length", PDFNumber 1.0)]) ""
    , setStream "abcd"
    , PDFObjectStream 7 8 (Map.fromList [("Length", PDFNumber 4.0)]) "abcd"
    )
  ]

embedObjectExamples :: [(PDFObject, FallibleComputation (), PDFObject)]
embedObjectExamples =
  [ (PDFArray [], embedObject (PDFArray [PDFName "a"]), PDFArray [PDFName "a"])
  , ( PDFIndirectObject 1 0 PDFNull
    , embedObject (PDFArray [PDFName "a"])
    , PDFIndirectObject 1 0 (PDFArray [PDFName "a"])
    )
  , ( PDFObjectStream 8 5 Map.empty ""
    , embedObject (PDFDictionary (Map.fromList [("a", PDFNull)]))
    , PDFObjectStream 8 5 (Map.fromList [("a", PDFNull)]) ""
    )
  ]

spec :: Spec
spec = do
  describe "setMaybe" $ forM_ setMaybeExamples $ \(example, fn, expected) ->
    it ("should work on " ++ show example)
      $          update example fn
      `shouldBe` expected

  describe "setStream" $ forM_ setStreamExamples $ \(example, fn, expected) ->
    it ("should work on " ++ show example)
      $          update example fn
      `shouldBe` expected

  describe "embedObject"
    $ forM_ embedObjectExamples
    $ \(example, fn, expected) ->
        it ("should work on " ++ show example)
          $          updateE example fn
          `shouldBe` Right expected
