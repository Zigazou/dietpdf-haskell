module PDF.Document.AnalyzeBitmapMasksSpec (spec) where

import Codec.Compression.CCITTG4 (encodeG4)
import Codec.Compression.Flate qualified as Flate
import Control.Monad (forM_)
import Data.Bitmap.MaskAnalysis (MaskClass (BinaryMask, NearlyBinaryMask, SmoothMask, DetailedMask), MaskThresholds (nearlyBinaryFraction), MaskAnalysis (maskClass, distinctValues, intermediateFraction, intermediateBounds, largeDifferenceFraction, opaqueFraction, pixelCount), PixelBounds (PixelBounds), analyzeMask, defaultMaskThresholds)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.PDF.PDFDocument (fromList)
import Data.PDF.PDFObject (PDFObject (PDFArray, PDFBool, PDFDictionary, PDFIndirectObject, PDFIndirectObjectWithStream, PDFName, PDFNumber, PDFReference))
import Data.PDF.PDFWork (evalPDFWorkT, getObject)
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import PDF.Document.AnalyzeBitmapMasks (BitmapMaskRole (SoftMask, ExplicitMask, StencilMask), BitmapMaskInfo (bitmapMaskRoles, bitmapMaskAnalysis), analyzeBitmapMasks, readBitmapMask)
import PDF.Processing.PDFWork (importObjects)
import Test.Hspec (Spec, describe, it, shouldBe, expectationFailure)

spec :: Spec
spec = describe "Bitmap mask analysis" $ do
  it "classifies binary and constant masks" $ do
    fmap maskClass (analyzeMask defaultMaskThresholds 2 1 (BS.pack [0,255])) `shouldBe` Right BinaryMask
    fmap maskClass (analyzeMask defaultMaskThresholds 2 1 (BS.pack [128,128])) `shouldBe` Right SmoothMask
  it "records histogram fractions and inclusive intermediate bounds" $ do
    let result = analyzeMask defaultMaskThresholds 3 2 (BS.pack [0,10,255,0,20,255])
    fmap distinctValues result `shouldBe` Right 4
    fmap intermediateFraction result `shouldBe` Right (1 / 3)
    fmap intermediateBounds result `shouldBe` Right (Just (PixelBounds 1 0 1 1))
  it "distinguishes nearly binary masks from detailed continuous masks" $ do
    fmap maskClass (analyzeMask defaultMaskThresholds 200 1 (BS.pack (128 : replicate 199 0))) `shouldBe` Right NearlyBinaryMask
    fmap maskClass (analyzeMask defaultMaskThresholds 4 1 (BS.pack [0,128,255,128])) `shouldBe` Right DetailedMask
  it "does not compare the last pixel of a row with the next row's first pixel" $ do
    fmap largeDifferenceFraction (analyzeMask defaultMaskThresholds 2 2 (BS.pack [0,100,0,100])) `shouldBe` Right 0.5
  it "rejects empty, truncated and overflowing geometry" $ do
    analyzeMask defaultMaskThresholds 0 1 BS.empty `shouldBe` Left "Invalid mask dimensions"
    analyzeMask defaultMaskThresholds 2 2 "a" `shouldBe` Left "Invalid mask sample count"
    analyzeMask defaultMaskThresholds maxBound maxBound "a" `shouldBe` Left "Invalid mask sample count"
  it "detects and analyzes a shared Flate soft mask once" $ do
    let raw = BS.pack [0,128,255]
        compressed = either (error . show) id (Flate.compress raw)
        mask = image 2 [("ColorSpace", PDFName "DeviceGray"), ("BitsPerComponent", PDFNumber 8), ("Filter", PDFName "FlateDecode")] compressed
        owner = image 1 [("SMask", PDFReference 2 0)] ""
    result <- evalPDFWorkT $ importObjects (fromList [owner, mask, image 3 [("SMask", PDFReference 2 0)] ""]) >> analyzeBitmapMasks defaultMaskThresholds
    case result of
      Right [info] -> do
        bitmapMaskRoles info `shouldBe` Set.singleton SoftMask
        fmap maskClass (bitmapMaskAnalysis info) `shouldBe` Right DetailedMask
      _ -> expectationFailure (show result)
  it "decodes PNG predictors without changing the stored mask" $ do
    let compressed = either (error . show) id (Flate.compress (BS.pack [1,0,128,127]))
        params = PDFDictionary (Map.fromList [("Predictor",PDFNumber 15),("Columns",PDFNumber 3)])
        mask = image 2 [("ColorSpace",PDFName "DeviceGray"),("BitsPerComponent",PDFNumber 8),("Filter",PDFName "FlateDecode"),("DecodeParms",params)] compressed
        owner = image 1 [("SMask",PDFReference 2 0)] ""
    result <- evalPDFWorkT $ do
      importObjects (fromList [owner,mask])
      infos <- analyzeBitmapMasks defaultMaskThresholds
      stored <- getObject 2
      return (infos,stored)
    case result of
      Right ([info],stored) -> do
        stored `shouldBe` Just mask
        bitmapMaskAnalysis info `shouldBe` analyzeMask defaultMaskThresholds 3 1 (BS.pack [0,128,255])
      _ -> expectationFailure (show result)
  it "decodes four-bit soft masks with odd-row padding and reversed Decode" $ do
    let mask = image 2 [("ColorSpace",PDFName "DeviceGray"),
          ("BitsPerComponent",PDFNumber 4),("Height",PDFNumber 2),
          ("Decode",PDFArray (Seq.fromList [PDFNumber 1,PDFNumber 0]))]
          (BS.pack [0x01,0xff,0xf1,0x0f])
    result <- evalPDFWorkT (readBitmapMask (Set.singleton SoftMask) mask)
    result `shouldBe` Right (Right (3,2,BS.pack [255,238,0,0,238,255]))
  it "rejects invalid thresholds" $
    analyzeMask (defaultMaskThresholds { nearlyBinaryFraction = 0 / 0 }) 1 1 "a" `shouldBe` Left "Invalid mask thresholds"
  it "ignores padding bits and applies stencil polarity and reversed Decode" $ do
    let mask = image 2 [("ImageMask", PDFBool True), ("Decode", PDFArray (Seq.fromList [PDFNumber 1,PDFNumber 0]))] (BS.pack [0x7f])
        owner = image 1 [("Mask", PDFReference 2 0)] ""
    result <- evalPDFWorkT $ importObjects (fromList [owner,mask]) >> analyzeBitmapMasks defaultMaskThresholds
    case result of
      Right [info] -> do
        bitmapMaskRoles info `shouldBe` Set.fromList [ExplicitMask,StencilMask]
        fmap opaqueFraction (bitmapMaskAnalysis info) `shouldBe` Right (2 / 3)
        fmap pixelCount (bitmapMaskAnalysis info) `shouldBe` Right 3
      _ -> expectationFailure (show result)
  it "excludes color-key masks, ordinary grayscale images and soft-mask groups" $ do
    let owner = image 1 [("Mask", PDFArray (Seq.fromList [PDFNumber 0,PDFNumber 1])), ("SMask", PDFReference 3 0)] ""
        group = PDFIndirectObject 3 0 (PDFDictionary (Map.singleton "S" (PDFName "Alpha")))
    result <- evalPDFWorkT $ importObjects (fromList [owner, image 2 [("ColorSpace",PDFName "DeviceGray")] "", group]) >> analyzeBitmapMasks defaultMaskThresholds
    result `shouldBe` Right []
  it "decodes supported Group-4 streams with either BlackIs1 polarity" $ do
    let encoded = either (error . show) id (encodeG4 3 1 (BS.pack [0xa0]))
    forM_ [False,True] $ \blackIs1 -> do
      let params = PDFDictionary (Map.fromList
            [("K",PDFNumber (-1)),("Columns",PDFNumber 3),("Rows",PDFNumber 1),
             ("EndOfBlock",PDFBool False),("BlackIs1",PDFBool blackIs1)])
          mask = image 2 [("ColorSpace",PDFName "DeviceGray"),
            ("BitsPerComponent",PDFNumber 1),("Filter",PDFName "CCITTFaxDecode"),
            ("DecodeParms",params)] encoded
          owner = image 1 [("SMask",PDFReference 2 0)] ""
          alpha = BS.pack (if blackIs1 then [255,0,255] else [0,255,0])
      result <- evalPDFWorkT $ importObjects (fromList [owner,mask]) >> analyzeBitmapMasks defaultMaskThresholds
      case result of
        Right [info] -> bitmapMaskAnalysis info `shouldBe` analyzeMask defaultMaskThresholds 3 1 alpha
        _ -> expectationFailure (show result)
  it "retains unsupported masks without classifying compressed bytes" $ do
    let mask = image 2 [("ImageMask", PDFBool True), ("Filter", PDFName "CCITTFaxDecode")] "x"
    result <- evalPDFWorkT $ importObjects (fromList [mask]) >> analyzeBitmapMasks defaultMaskThresholds
    case result of
      Right [info] -> bitmapMaskAnalysis info `shouldBe` Left "Unsupported mask filter"
      _ -> expectationFailure (show result)

image :: Int -> [(BS.ByteString, PDFObject)] -> BS.ByteString -> PDFObject
image number entries = PDFIndirectObjectWithStream number 0 (Map.fromList
  ([("Subtype",PDFName "Image"),("Width",PDFNumber 3),("Height",PDFNumber 1)] ++ entries))
