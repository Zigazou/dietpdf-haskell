module Data.Bitmap.OptimizeMaskSpec (spec) where

import Data.Bitmap.MaskAnalysis (analyzeMask, defaultMaskThresholds)
import Data.Bitmap.OptimizeMask (maskCandidates, packMask)
import Data.ByteString qualified as BS
import Data.Word (Word8)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = describe "Alpha candidates" $ do
  it "packs odd-width rows independently with zero padding" $
    packMask 3 2 (BS.pack [255,0,255,0,255,0]) `shouldBe` BS.pack [0xa0,0x40]
  it "leaves all samples unchanged in lossless mode" $ do
    let raw = BS.pack [0,1,2,3,100,253,254,255]
    candidates False 8 1 raw `shouldBe` [(8,1,raw)]
  it "snaps only near extremes and keeps every candidate within the error bound" $ do
    let raw = BS.pack [0..255]
        variants = candidates True 256 1 raw
    variants `shouldSatisfy` (elem (256,1,BS.map snap raw))
    mapM_ (\(_,_,samples) -> BS.zipWith
      (\a b -> abs (fromIntegral a - (fromIntegral b :: Int))) raw samples
      `shouldSatisfy` all (<= 3)) variants
  it "does not binarize a rare mid-alpha detail" $ do
    let raw = BS.pack (128 : replicate 199 0)
    candidates True 200 1 raw `shouldSatisfy`
      all (\(w,h,samples) -> w == 200 && h == 1 && BS.head samples > 120)
  it "reduces a smooth constant mask by two and four" $ do
    let variants = candidates True 16 16 (BS.replicate 256 128)
    variants `shouldSatisfy` elem (8,8,BS.replicate 64 128)
    variants `shouldSatisfy` elem (4,4,BS.replicate 16 128)
  it "preserves sharp edges, exact transparent pixels and odd geometry" $ do
    let edge = BS.pack (concat (replicate 16 (replicate 8 50 ++ replicate 8 200)))
    candidates True 16 16 edge `shouldSatisfy` all (\(w,h,_) -> w == 16 && h == 16)
    candidates True 16 16 (BS.replicate 256 0) `shouldSatisfy` all (\(w,h,_) -> w == 16 && h == 16)
    candidates True 3 3 (BS.replicate 9 128) `shouldSatisfy` all (\(w,h,_) -> w == 3 && h == 3)
 where
  snap :: Word8 -> Word8
  snap value | value <= 3 = 0
             | value >= 252 = 255
             | otherwise = value

candidates :: Bool -> Int -> Int -> BS.ByteString -> [(Int, Int, BS.ByteString)]
candidates lossy width height raw = case analyzeMask defaultMaskThresholds width height raw of
  Left reason -> error reason
  Right analysis -> maskCandidates lossy analysis width height raw
