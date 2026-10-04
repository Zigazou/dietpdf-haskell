module PDF.Object.OptimizeSpec
  ( spec
  ) where

import Codec.Compression.RunLength qualified as RL
import Codec.Compression.Zlib qualified as ZL

import Control.Monad (forM_)

import Data.ByteString.Lazy qualified as BL
import Data.Either.Extra (fromRight)
import Data.Map.Strict qualified as Map
import Data.PDF.PDFObject
    ( PDFObject (PDFIndirectObjectWithStream, PDFName, PDFNumber, PDFHexString)
    , mkPDFArray
    )
import Data.PDF.PDFWork (evalPDFWorkT)

import PDF.Processing.Optimize (optimize)
import PDF.Processing.Unfilter (unfilter)

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
spec = do
  describe "optimize" $ forM_ objectExamples $ \(example, expected) ->
    it ("should be optimized " ++ show example) $ do
      optimized <- evalPDFWorkT (optimize Nothing example)
      optimized `shouldBe` Right expected

  describe "grayscale bitmap optimization" $ do
    it "updates the bit depth, palette and packed RGB image stream" $ do
      let image = PDFIndirectObjectWithStream 2 0
            (Map.fromList
              [ ("Subtype", PDFName "Image")
              , ("Width", PDFNumber 3), ("Height", PDFNumber 1)
              , ("BitsPerComponent", PDFNumber 8)
              , ("ColorSpace", PDFName "DeviceRGB")
              ]) "aaabbbaaa"
          expected = PDFIndirectObjectWithStream 2 0
            (Map.fromList
              [ ("Subtype", PDFName "Image")
              , ("Width", PDFNumber 3), ("Height", PDFNumber 1)
              , ("Length", PDFNumber 1)
              , ("BitsPerComponent", PDFNumber 1)
              , ("ColorSpace", mkPDFArray
                  [PDFName "Indexed", PDFName "DeviceGray", PDFNumber 1,
                   PDFHexString "ab"])
              ]) "@"
      result <- evalPDFWorkT (optimize Nothing image >>= unfilter)
      result `shouldBe` Right expected
