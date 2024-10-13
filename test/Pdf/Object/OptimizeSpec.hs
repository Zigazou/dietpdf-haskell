module Pdf.Object.OptimizeSpec
  ( spec
  ) where

import Codec.Compression.RunLength qualified as RL
import Codec.Compression.Zlib qualified as ZL

import Control.Monad (forM_)

import Data.ByteString.Lazy qualified as BL
import Data.Either.Extra (fromRight)
import Data.Map.Strict qualified as Map
import Data.PDF.PDFObject
    ( PDFObject (PDFIndirectObjectWithStream, PDFName, PDFNumber)
    )
import Data.PDF.PDFWork (evalPDFWorkT)

import Pdf.Processing.Optimize (optimize)

import Test.Hspec (Spec, describe, it, shouldBe)


objectExamples :: [(PDFObject, PDFObject)]
objectExamples =
  [ ( PDFIndirectObjectWithStream
      1
      0
      (Map.fromList
        [("Size", PDFNumber 16.0), ("Filter", PDFName "FlateDecode")]
      )
      (BL.toStrict . ZL.compress . BL.fromStrict $ "Hello, world!")
    , PDFIndirectObjectWithStream
      1
      0
      (Map.fromList
        [("Size", PDFNumber 16.0), ("Filter", PDFName "RunLengthDecode")]
      )
      (fromRight "" $ RL.compress "Hello, world!")
    )
  ]

spec :: Spec
spec = describe "optimize" $ forM_ objectExamples $ \(example, expected) ->
  it ("should be optimized " ++ show example) $ do
    optimized <- evalPDFWorkT (optimize example)
    optimized `shouldBe` Right expected
