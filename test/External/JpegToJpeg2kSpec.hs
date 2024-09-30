module External.JpegToJpeg2kSpec
  ( spec
  ) where

import Data.ByteString qualified as BS

import External.JpegToJpeg2k (jpegToJpeg2k)

import Test.Hspec (Spec, describe, it, shouldBe)
import Control.Monad.Except (runExceptT)

spec :: Spec
spec = do
  describe "jpegtojpeg2k" $ do
    it "converts jpeg to jpeg2k" $ do
      jpegData <- BS.readFile "test/External/jpegtojpeg2k.opt.jpg"
      jpeg2kData <- runExceptT $ jpegToJpeg2k 15 jpegData
      BS.take 6 <$> jpeg2kData `shouldBe` Right "\x00\x00\x00\x0c\x6a\x50"
