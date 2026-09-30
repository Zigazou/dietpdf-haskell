module PDF.Document.OptimizeBitmapMasksSpec (spec) where

import Control.Monad.State (modify)
import Control.Monad (forM_, when)
import Data.Bitmap.BitmapConfiguration (BitmapConfiguration (BitmapConfiguration))
import Data.Bitmap.BitsPerComponent (BitsPerComponent (BC1Bit, BC8Bits))
import Data.Bitmap.OptimizeMask (packMask)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.PDF.PDFDocument (fromList)
import Data.PDF.FilterCombination (FilterCombination (fcBytes, fcList))
import Data.PDF.PDFObject (PDFObject (PDFIndirectObjectWithStream, PDFName, PDFNumber, PDFBool, PDFArray, PDFReference))
import Data.PDF.PDFWork (evalPDFWorkT, getObject)
import Data.PDF.Settings (defaultSettings, sLossyMasks, sCompressor, UseCompressor (UseDeflate, UseECT, UseZopfli))
import Data.PDF.WorkData (WorkData (wSettings))
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import PDF.Document.AnalyzeBitmapMasks (BitmapMaskRole (SoftMask, StencilMask), readBitmapMask)
import PDF.Document.OptimizeBitmapMasks (optimizeBitmapMasks)
import PDF.Object.Object.FromPDFObject (fromPDFObject)
import PDF.Object.Object.Properties (getValueForKey)
import PDF.Object.Container (setFilters)
import PDF.Processing.FilterCombine.PredRleCompressor (predRleCompressor)
import PDF.Processing.FilterCombine.RleCompressor (rleCompressor)
import PDF.Processing.PDFWork (importObjects)
import System.Directory.Extra (findExecutable)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy, expectationFailure, pendingWith)

spec :: Spec
spec = describe "Bitmap mask optimization" $ do
  forM_ [("Deflate", UseDeflate), ("ECT", UseECT), ("Zopfli", UseZopfli)] $ \(name, compression) ->
    forM_ [False, True] $ \binary ->
      it ("compares RLE chains and preserves alpha with " ++ name
          ++ if binary then " for binary masks" else " for continuous masks") $ do
        when (compression == UseECT) $ do
          executable <- findExecutable "ect"
          when (executable == Nothing) (pendingWith "ECT executable is not installed")
        let width, height :: Int
            width = 257
            height = 64
            values = take (width * height)
              [fromIntegral ((n * 73) `mod` 256) | n <- [0..] :: [Int], _ <- [1..37 :: Int]]
            raw = BS.pack (if binary then map (\v -> if v < 128 then 0 else 255) values else values)
            bits = if binary then BC1Bit else BC8Bits
            packed = if binary then packMask width height raw else raw
            config = BitmapConfiguration width 1 bits
            entries :: [(BS.ByteString, PDFObject)]
            entries = [("ColorSpace",PDFName "DeviceGray"),
                       ("BitsPerComponent",PDFNumber 8)]
            original = image 2 width height entries raw
        result <- optimizeWith compression False SoftMask original
        case result of
          Right (Just best, decoded) -> do
            decoded `shouldBe` Right (width,height,raw)
            BS.length (fromPDFObject best) `shouldSatisfy` (<= BS.length (fromPDFObject original))
            forM_ [rleCompressor, predRleCompressor] $ \encode -> do
              encoded <- either (fail . show) pure (encode (Just config) packed compression)
              candidate <- evalPDFWorkT (setFilters (fcList encoded)
                (image 2 width height
                  [("ColorSpace",PDFName "DeviceGray"),
                   ("BitsPerComponent",PDFNumber (if binary then 1 else 8))]
                  (fcBytes encoded))) >>= either (fail . show) pure
              -- Include dictionary overhead when comparing the complete chains.
              BS.length (fromPDFObject best) `shouldSatisfy` (<= BS.length (fromPDFObject candidate))
              roundTrip <- evalPDFWorkT (readBitmapMask (Set.singleton SoftMask) candidate)
              roundTrip `shouldBe` Right (Right (width,height,raw))
          _ -> expectationFailure (show result)
  it "converts binary soft masks to one bit and preserves reversed Decode" $ do
    let raw = BS.pack (concat (replicate 64 (replicate 32 0 ++ replicate 33 255)))
        original = image 2 65 64 [("ColorSpace",PDFName "DeviceGray"),
          ("BitsPerComponent",PDFNumber 8),
          ("Decode",PDFArray (Seq.fromList [PDFNumber 1,PDFNumber 0]))] raw
    result <- optimize False SoftMask original
    case result of
      Right (Just best, decoded) -> do
        getValueForKey "BitsPerComponent" best `shouldBe` Just (PDFNumber 1)
        getValueForKey "ImageMask" best `shouldBe` Nothing
        decoded `shouldBe` Right (65,64,BS.map (255 -) raw)
        BS.length (fromPDFObject best) `shouldSatisfy` (< BS.length (fromPDFObject original))
      _ -> expectationFailure (show result)
  it "preserves stencil painting polarity and odd-width padding" $ do
    let original = image 2 65 64 [("ImageMask",PDFBool True)] (BS.replicate (9 * 64) 0)
    result <- optimize False StencilMask original
    case result of
      Right (Just _,decoded) -> decoded `shouldBe` Right (65,64,BS.replicate (65 * 64) 255)
      _ -> expectationFailure (show result)
  it "snaps near-binary alpha only when lossy masks are enabled" $ do
    let noise :: [Integer]
        noise = drop 1 (iterate (\n -> (1103515245 * n + 12345) `mod` 2147483648) 42)
        raw = BS.pack (take (65 * 64)
          [ [1,2,253,254,0,255,3,252] !! fromInteger ((n `div` 65536) `mod` 8)
          | n <- noise])
        original = image 2 65 64 [("ColorSpace",PDFName "DeviceGray"),
          ("BitsPerComponent",PDFNumber 8)] raw
    lossless <- optimize False SoftMask original
    lossy <- optimize True SoftMask original
    case (lossless, lossy) of
      (Right (_,plain), Right (Just best, changed)) -> do
        plain `shouldBe` Right (65,64,raw)
        getValueForKey "BitsPerComponent" best `shouldBe` Just (PDFNumber 1)
        changed `shouldBe` Right (65,64,BS.map (\v -> if v < 128 then 0 else 255) raw)
      _ -> expectationFailure (show (lossless,lossy))
  it "keeps tiny masks when dictionary overhead outweighs compression" $ do
    let original = image 2 1 1 [("ColorSpace",PDFName "DeviceGray"),("BitsPerComponent",PDFNumber 8)] "x"
    result <- optimize False SoftMask original
    fmap fst result `shouldBe` Right (Just original)
  it "round-trips continuous samples exactly by default" $ do
    let raw = BS.pack (concat (replicate 64 [0..255]))
        original = image 2 256 64 [("ColorSpace",PDFName "DeviceGray"),("BitsPerComponent",PDFNumber 8)] raw
    result <- optimize False SoftMask original
    case result of
      Right (Just _,decoded) -> decoded `shouldBe` Right (256,64,raw)
      _ -> expectationFailure (show result)
  it "keeps unsupported and malformed masks unchanged" $ do
    let entries :: [(BS.ByteString, PDFObject)]
        entries = [("ColorSpace",PDFName "DeviceGray"),("BitsPerComponent",PDFNumber 8)]
    mapM_ (\original -> do
      result <- optimize True SoftMask original
      fmap fst result `shouldBe` Right (Just original))
      [image 2 64 64 entries "short",
       image 2 64 64 (("Filter",PDFName "DCTDecode") : entries) "unsupported"]
  it "does not quantize Matte masks even with the lossy option" $ do
    let raw = BS.pack (concat (replicate 64 [0..255]))
        original = image 2 256 64 [("ColorSpace",PDFName "DeviceGray"),
          ("BitsPerComponent",PDFNumber 8),("Matte",PDFArray (Seq.singleton (PDFNumber 0)))] raw
    result <- optimize True SoftMask original
    case result of
      Right (Just _,decoded) -> decoded `shouldBe` Right (256,64,raw)
      _ -> expectationFailure (show result)
 where
  optimize = optimizeWith UseDeflate
  optimizeWith compression lossy role original = evalPDFWorkT $ do
    modify (\state -> state {wSettings = defaultSettings {sLossyMasks = lossy, sCompressor = compression}})
    importObjects (fromList [image 1 1 1 [(if role == SoftMask then "SMask" else "Mask",PDFReference 2 0)] "", original])
    _ <- optimizeBitmapMasks
    best <- getObject 2
    decoded <- maybe (return (Left "Missing mask")) (readBitmapMask (Set.singleton role)) best
    return (best,decoded)

image :: Int -> Int -> Int -> [(BS.ByteString, PDFObject)] -> BS.ByteString -> PDFObject
image number width height entries raw = PDFIndirectObjectWithStream number 0 (Map.fromList
  ([("Subtype",PDFName "Image"),("Width",PDFNumber (fromIntegral width)),
    ("Height",PDFNumber (fromIntegral height)),("Length",PDFNumber (fromIntegral (BS.length raw)))] ++ entries)) raw
