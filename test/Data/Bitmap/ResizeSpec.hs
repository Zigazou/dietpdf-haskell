module Data.Bitmap.ResizeSpec (spec) where

import Data.Bitmap.Resize (resizeBitmap)
import Data.ByteString qualified as BS
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Area-average bitmap resizing" $ do
  it "averages every source pixel instead of selecting nearest neighbors" $
    resizeBitmap 2 2 1 8 1 1 (BS.pack [0, 100, 200, 100]) `shouldBe` Just (BS.pack [100])
  it "weights fractional source edges" $
    resizeBitmap 3 1 1 8 2 1 (BS.pack [0, 90, 240]) `shouldBe` Just (BS.pack [30, 190])
  it "keeps interleaved color channels separate" $
    resizeBitmap 2 1 3 8 1 1 (BS.pack [0, 20, 40, 100, 120, 140])
      `shouldBe` Just (BS.pack [50, 70, 90])
  it "averages full big-endian 16-bit samples" $
    resizeBitmap 2 1 1 16 1 1 (BS.pack [0, 0, 255, 255])
      `shouldBe` Just (BS.pack [128, 0])
  it "rejects malformed buffers, packed bits, zero sizes and upsampling" $ do
    resizeBitmap 2 1 1 8 1 1 "x" `shouldBe` Nothing
    resizeBitmap 2 1 1 4 1 1 "x" `shouldBe` Nothing
    resizeBitmap 2 1 1 8 0 1 "xy" `shouldBe` Nothing
    resizeBitmap 2 1 1 8 3 1 "xy" `shouldBe` Nothing
