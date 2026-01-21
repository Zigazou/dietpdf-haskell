{-# LANGUAGE OverloadedStrings #-}

module PDF.Processing.UnfilterSpec
  ( spec
  ) where

import Codec.Compression.Flate qualified as FL
import Codec.Compression.LZW qualified as LZ
import Codec.Compression.Predict
  (Entropy (EntropyDeflate), Predictor (TIFFPredictor2), predict)
import Codec.Compression.RunLength qualified as RL
import Codec.Filter.Ascii85 qualified as A8
import Codec.Filter.AsciiHex qualified as AH

import Data.Bitmap.BitmapConfiguration qualified as BC
import Data.Bitmap.BitsPerComponent qualified as BPC
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.PDF.Filter (Filter (Filter))
import Data.PDF.FilterList (mkFilterList)
import Data.PDF.PDFObject
  ( PDFObject (PDFIndirectObject, PDFIndirectObjectWithStream, PDFName, PDFNull, PDFNumber)
  )
import Data.PDF.PDFWork (evalPDFWorkT)

import PDF.Object.Container (getFilters, setFilters)
import PDF.Object.Object (mkPDFDictionary)
import PDF.Object.State (getStream, setStream)
import PDF.Processing.Unfilter (unfilter)

import Test.Hspec (Spec, describe, it, shouldBe)

-- Helpers
rightOrFail :: Show e => Either e a -> IO a
rightOrFail = either (\e -> fail ("Unexpected error: " ++ show e)) pure

mkStreamObject :: ByteString -> PDFObject
mkStreamObject = PDFIndirectObjectWithStream 1 0 mempty

setFiltersAndStream :: PDFObject -> [Filter] -> ByteString -> IO PDFObject
setFiltersAndStream obj filters bs = do
  objWithStream <- evalPDFWorkT (setStream bs obj)
    >>= rightOrFail
  evalPDFWorkT (setFilters (mkFilterList filters) objWithStream)
    >>= rightOrFail

spec :: Spec
spec = do
  describe "unfilter" $ do
    it "returns the object unchanged when it has no stream" $ do
      let obj = PDFIndirectObject 1 0 (mkPDFDictionary [("A", PDFNumber 1.0)])
      result <- evalPDFWorkT (unfilter obj)
      result `shouldBe` Right obj

    it "decodes FlateDecode streams" $ do
      let original = "hello hello hello" :: ByteString
      compressed <- rightOrFail (FL.compress original)
      obj0 <- setFiltersAndStream (mkStreamObject BS.empty)
                                  [Filter (PDFName "FlateDecode") PDFNull]
                                  compressed
      resultObj <- evalPDFWorkT (unfilter obj0) >>= rightOrFail
      finalStream <- evalPDFWorkT (getStream resultObj) >>= rightOrFail
      finalFilters <- evalPDFWorkT (getFilters resultObj) >>= rightOrFail
      finalStream `shouldBe` original
      -- All supported filters should be consumed
      finalFilters `shouldBe` mempty

    it "decodes RunLengthDecode streams" $ do
      let original = "AAAABBBCCDDEEEEEEEEEEEE" :: ByteString
      encoded <- rightOrFail (RL.compress original)
      obj0 <- setFiltersAndStream (mkStreamObject BS.empty)
                                  [Filter (PDFName "RunLengthDecode") PDFNull]
                                  encoded
      resultObj <- evalPDFWorkT (unfilter obj0) >>= rightOrFail
      finalStream <- evalPDFWorkT (getStream resultObj) >>= rightOrFail
      finalFilters <- evalPDFWorkT (getFilters resultObj) >>= rightOrFail
      finalStream `shouldBe` original
      finalFilters `shouldBe` mempty

    it "decodes LZWDecode streams" $ do
      let original = "LZW LZW LZW test data" :: ByteString
      encoded <- rightOrFail (LZ.compress original)
      obj0 <- setFiltersAndStream (mkStreamObject BS.empty)
                                  [Filter (PDFName "LZWDecode") PDFNull]
                                  encoded
      resultObj <- evalPDFWorkT (unfilter obj0) >>= rightOrFail
      finalStream <- evalPDFWorkT (getStream resultObj) >>= rightOrFail
      finalFilters <- evalPDFWorkT (getFilters resultObj) >>= rightOrFail
      finalStream `shouldBe` original
      finalFilters `shouldBe` mempty

    it "decodes ASCII85Decode streams" $ do
      let original = "abcdefg12345\0\1\2" :: ByteString
      encoded <- rightOrFail (A8.encode original)
      obj0 <- setFiltersAndStream (mkStreamObject BS.empty)
                                  [Filter (PDFName "ASCII85Decode") PDFNull]
                                  encoded
      resultObj <- evalPDFWorkT (unfilter obj0) >>= rightOrFail
      finalStream <- evalPDFWorkT (getStream resultObj) >>= rightOrFail
      finalFilters <- evalPDFWorkT (getFilters resultObj) >>= rightOrFail
      finalStream `shouldBe` original
      finalFilters `shouldBe` mempty

    it "decodes ASCIIHexDecode streams" $ do
      let original = "\x12\x34\x56\x78\x9a\xbc\xde\xf0" :: ByteString
      encoded <- rightOrFail (AH.encode original)
      obj0 <- setFiltersAndStream (mkStreamObject BS.empty)
                                  [Filter (PDFName "ASCIIHexDecode") PDFNull]
                                  encoded
      resultObj <- evalPDFWorkT (unfilter obj0) >>= rightOrFail
      finalStream <- evalPDFWorkT (getStream resultObj) >>= rightOrFail
      finalFilters <- evalPDFWorkT (getFilters resultObj) >>= rightOrFail
      finalStream `shouldBe` original
      finalFilters `shouldBe` mempty

    it "decodes FlateDecode with TIFF predictor (Predictor=2)" $ do
      let original = "\x00\x01\x02\x03\x04\x05\x06\x07" :: ByteString
          columns  = BS.length original
          decodeParms = mkPDFDictionary
            [ ("Predictor",       PDFNumber 2.0)
            , ("Columns",         PDFNumber (fromIntegral columns))
            , ("BitsPerComponent", PDFNumber 8.0)
            , ("Colors",           PDFNumber 1.0)
            ]
      predicted <- rightOrFail (predict EntropyDeflate TIFFPredictor2
                                      (mkBitmapConfig columns)
                                      original)
      compressed <- rightOrFail (FL.compress predicted)
      obj0 <- setFiltersAndStream (mkStreamObject BS.empty)
                                  [Filter (PDFName "FlateDecode") decodeParms]
                                  compressed
      resultObj <- evalPDFWorkT (unfilter obj0) >>= rightOrFail
      finalStream <- evalPDFWorkT (getStream resultObj) >>= rightOrFail
      finalFilters <- evalPDFWorkT (getFilters resultObj) >>= rightOrFail
      finalStream `shouldBe` original
      finalFilters `shouldBe` mempty

    it "decodes a filter chain [ASCII85Decode, FlateDecode]" $ do
      let original = "chain test payload" :: ByteString
      deflated <- rightOrFail (FL.compress original)
      ascii85  <- rightOrFail (A8.encode deflated)
      obj0 <- setFiltersAndStream (mkStreamObject BS.empty)
                                  [ Filter (PDFName "ASCII85Decode") PDFNull
                                  , Filter (PDFName "FlateDecode")   PDFNull
                                  ]
                                  ascii85
      resultObj <- evalPDFWorkT (unfilter obj0) >>= rightOrFail
      finalStream <- evalPDFWorkT (getStream resultObj) >>= rightOrFail
      finalFilters <- evalPDFWorkT (getFilters resultObj) >>= rightOrFail
      finalStream `shouldBe` original
      finalFilters `shouldBe` mempty

    it "preserves unsupported filters (e.g., DCTDecode)" $ do
      let original = "raw-bytes" :: ByteString
      obj0 <- setFiltersAndStream (mkStreamObject original)
                                  [Filter (PDFName "DCTDecode") PDFNull]
                                  original
      resultObj <- evalPDFWorkT (unfilter obj0) >>= rightOrFail
      finalStream <- evalPDFWorkT (getStream resultObj) >>= rightOrFail
      finalFilters <- evalPDFWorkT (getFilters resultObj) >>= rightOrFail
      -- Stream unchanged and filters preserved
      finalStream `shouldBe` original
      finalFilters `shouldBe` mkFilterList [Filter (PDFName "DCTDecode") PDFNull]
  where
    -- Minimal bitmap config for generating predictor test data
    mkBitmapConfig :: Int -> BC.BitmapConfiguration
    mkBitmapConfig w = BC.BitmapConfiguration
      { BC.bcLineWidth = w
      , BC.bcComponents = 1
      , BC.bcBitsPerComponent = BPC.BC8Bits
      }
