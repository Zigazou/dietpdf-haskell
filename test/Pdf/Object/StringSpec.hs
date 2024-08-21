module Pdf.Object.StringSpec
  ( spec
  ) where

import Control.Monad (forM_)
import Control.Monad.Trans.Except (runExceptT)

import Data.ByteString qualified as BS

import Pdf.Object.Object (PDFObject (PDFHexString, PDFString), fromPDFObject)
import Pdf.Object.String (optimizeString)

import Test.Hspec (Spec, describe, it, shouldBe)

stringExamples :: [(PDFObject, BS.ByteString)]
stringExamples =
  [ (PDFString ""             , "()")
  , (PDFString "()"           , "(\\(\\))")
  , (PDFString "Hello, World!", "(Hello, World!)")
  ]

optimizeStringExamples :: [(PDFObject, PDFObject)]
optimizeStringExamples =
  [ (PDFHexString "48656C6C6F2C20576F726C6421", PDFString "Hello, World!")
  , ( PDFHexString "FEFF00480065006C006C006F002C00200057006F0072006C00640021"
    , PDFString "Hello, World!"
    )
  , ( PDFHexString "feff0046007200e9006400e9007200690063"
    , PDFString "Fr\233d\233ric"
    )
  , (PDFHexString "feff005", PDFString "P")
  ]

spec :: Spec
spec = describe "PDFString" $ do
  forM_ stringExamples $ \(example, expected) ->
    it ("should convert to bytestring " ++ show example)
      $          fromPDFObject example
      `shouldBe` expected

  forM_ optimizeStringExamples $ \(example, expected) ->
    it ("should convert to PDFString " ++ show example) $ do
      result <- runExceptT (optimizeString example)
      result `shouldBe` Right expected
