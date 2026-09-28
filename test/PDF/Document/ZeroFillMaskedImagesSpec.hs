module PDF.Document.ZeroFillMaskedImagesSpec (spec) where

import Codec.Compression.Flate qualified as Flate

import Control.Monad (forM_)

import Data.Bits (shiftL, shiftR, xor)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Fallible (Fallible)
import Data.Word (Word32)
import Data.PDF.PDFDocument (fromList)
import Data.PDF.PDFObject
  (PDFObject (PDFIndirectObjectWithStream, PDFName, PDFNumber, PDFReference))
import Data.PDF.PDFWork (evalPDFWorkT)

import PDF.Document.ZeroFillMaskedImages (zeroFillMaskedImages)
import PDF.Object.State (getStream, getValue)
import PDF.Processing.PDFWork (importObjects)

import Test.Hspec (Spec, describe, it, shouldBe)
import Util.Dictionary (mkDictionary)

-- A xorshift pseudo-random byte stream, so the noisy part hidden by the mask
-- does not already compress well (unlike a short periodic pattern).
xorshift :: Word32 -> Word32
xorshift x0 = x3
 where
  x1 = x0 `xor` (x0 `shiftL` 13)
  x2 = x1 `xor` (x1 `shiftR` 17)
  x3 = x2 `xor` (x2 `shiftL` 5)

-- 64x64 8-bit gray image; the first half of the pixels look like noise
-- (this is the part hidden by the mask), the second half is opaque.
rawImage :: ByteString
rawImage =
  BS.pack (fromIntegral <$> take 4096 (drop 1 (iterate xorshift 2463534242)))

-- The first half of the mask is fully transparent, the second half fully
-- opaque.
rawMask :: ByteString
rawMask = BS.replicate 2048 0 <> BS.replicate 2048 255

zeroFilledImage :: ByteString
zeroFilledImage = BS.replicate 2048 0 <> BS.drop 2048 rawImage

flateCompress :: ByteString -> ByteString
flateCompress = either (error . show) id . Flate.compress

flateDecompress :: ByteString -> ByteString
flateDecompress = either (error . show) id . Flate.decompress

mkImage :: PDFObject -> Maybe PDFObject -> ByteString -> PDFObject
mkImage colorSpace mSMaskRef raw =
  PDFIndirectObjectWithStream
    10
    0
    ( mkDictionary
      $ [ ("Subtype"         , PDFName "Image")
        , ("Width"           , PDFNumber 64)
        , ("Height"          , PDFNumber 64)
        , ("BitsPerComponent", PDFNumber 8)
        , ("ColorSpace"      , colorSpace)
        , ("Filter"          , PDFName "FlateDecode")
        ]
      ++ maybe [] (\ref -> [("SMask", ref)]) mSMaskRef
    )
    (flateCompress raw)

mkMask :: Int -> Int -> ByteString -> PDFObject
mkMask number width raw =
  PDFIndirectObjectWithStream
    number
    0
    ( mkDictionary
      [ ("Subtype"         , PDFName "Image")
      , ("Width"           , PDFNumber (fromIntegral width))
      , ("Height"          , PDFNumber (fromIntegral (BS.length raw `div` width)))
      , ("BitsPerComponent", PDFNumber 8)
      , ("ColorSpace"      , PDFName "DeviceGray")
      , ("Filter"          , PDFName "FlateDecode")
      ]
    )
    (flateCompress raw)

-- | Optimize an image object, returning its (possibly unmodified) decoded
-- pixel bytes, its Filter and its DecodeParms.
run
  :: PDFObject
  -> [PDFObject]
  -> IO (Fallible (ByteString, Maybe PDFObject, Maybe PDFObject))
run image others = evalPDFWorkT $ do
  importObjects $ fromList (image : others)
  optimized <- zeroFillMaskedImages image
  compressed <- getStream optimized
  filterName <- getValue "Filter" optimized
  decodeParms <- getValue "DecodeParms" optimized
  return (flateDecompress compressed, filterName, decodeParms)

spec :: Spec
spec = describe "Zero-fill masked images" $ do
  it "zeroes color components of fully-transparent pixels and shrinks the stream" $ do
    let image = mkImage (PDFName "DeviceGray")
                        (Just (PDFReference 11 0))
                        rawImage
        mask  = mkMask 11 64 rawMask
    result <- run image [mask]
    result `shouldBe` Right (zeroFilledImage, Just (PDFName "FlateDecode"), Nothing)

  it "leaves images without SMask untouched" $ do
    let image = mkImage (PDFName "DeviceGray") Nothing rawImage
    result <- run image []
    result `shouldBe` Right (rawImage, Just (PDFName "FlateDecode"), Nothing)

  it "leaves images with an unsupported color space untouched" $ do
    let image = mkImage (PDFName "DeviceN") (Just (PDFReference 11 0)) rawImage
        mask  = mkMask 11 64 rawMask
    result <- run image [mask]
    result `shouldBe` Right (rawImage, Just (PDFName "FlateDecode"), Nothing)

  it "leaves images untouched when the mask has a different size" $ do
    let image = mkImage (PDFName "DeviceGray")
                        (Just (PDFReference 11 0))
                        rawImage
        mask  = mkMask 11 32 (BS.replicate 1024 0)
    result <- run image [mask]
    result `shouldBe` Right (rawImage, Just (PDFName "FlateDecode"), Nothing)

  it "leaves images untouched when the mask is fully opaque" $ do
    let image = mkImage (PDFName "DeviceGray")
                        (Just (PDFReference 11 0))
                        rawImage
        mask  = mkMask 11 64 (BS.replicate 4096 255)
    result <- run image [mask]
    result `shouldBe` Right (rawImage, Just (PDFName "FlateDecode"), Nothing)

  forM_ [("DeviceRGB", 3), ("DeviceCMYK", 4)] $ \(colorSpace, components) ->
    it ("preserves partially transparent components in " ++ show colorSpace) $ do
      let raw = BS.pack (fromIntegral <$> take (4096 * components)
                          (drop 1 (iterate xorshift 2463534242)))
          alpha = BS.pack (take 4096 (cycle [0, 0, 1, 255]))
          expected = BS.pack
            [if BS.index alpha (offset `div` components) == 0 then 0 else byte
            | (offset, byte) <- zip [0..] (BS.unpack raw)]
          image = mkImage (PDFName colorSpace) (Just (PDFReference 11 0)) raw
          mask = mkMask 11 64 alpha
      result <- run image [mask]
      result `shouldBe` Right (expected, Just (PDFName "FlateDecode"), Nothing)
