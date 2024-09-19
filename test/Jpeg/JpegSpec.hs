module Jpeg.JpegSpec
  ( spec
  ) where

import Control.Monad (forM_)

import Data.Binary.Parser (parseDetail)
import Data.ByteString qualified as BS

import Jpeg.Jpeg (Jpeg, encode, jpegP)
import Jpeg.Segment
    ( Segment (Application, DefineHuffmanTable, DefineQuantizationTable, EndOfImage, StartOfFrame, StartOfImage, StartOfScan)
    )

import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import Util.Array (mkArray)
import Util.ParserHelper (itWith)

jpegExamples :: [(FilePath, Jpeg)]
jpegExamples =
  [ ( "jpeg-8x8-color-progressive.jpg"
    , mkArray
        [ StartOfImage
        , Application 0 "JFIF\x00\x01\x01\x01\x01,\x01,\x00\x00" ""
        , DefineQuantizationTable "\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"
        , DefineQuantizationTable "\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"
        , StartOfFrame 2 "\b\x00\b\x00\b\x03\x01\x11\x00\x02\x11\x01\x03\x11\x01"
        , DefineHuffmanTable "\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01"
        , DefineHuffmanTable "\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01"
        , StartOfScan "\x03\x01\x00\x02\x10\x03\x10\x00\x00\x01" "C"
        , DefineHuffmanTable "\x10\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
        , StartOfScan "\x01\x01\x00\x01\x05\x02" "\x7f"
        , DefineHuffmanTable "\x11\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
        , StartOfScan "\x01\x03\x01\x01?\x01" "\x7f"
        , DefineHuffmanTable "\x11\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
        , StartOfScan "\x01\x02\x01\x01?\x01" "\x7f"
        , DefineHuffmanTable "\x10\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
        , StartOfScan "\x01\x01\x00\ACK?\x02" "\x7f"
        , DefineHuffmanTable "\x10\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
        , StartOfScan "\x01\x01\x00\x01?!" "\x7f"
        , StartOfScan "\x03\x01\x00\x02\x00\x03\x00\x00\x00\x10" "\xff"
        , DefineHuffmanTable "\x11\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf0\xe1"
        , StartOfScan "\x01\x03\x01\x01?\x10" "\x13"
        , DefineHuffmanTable "\x11\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf0\xe1"
        , StartOfScan "\x01\x02\x01\x01?\x10" "\x13"
        , DefineHuffmanTable "\x10\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf0\xe1"
        , StartOfScan "\x01\x01\x00\x01?\x10" "\x13"
        , EndOfImage
        ]
    )
  , ( "jpeg-8x8-color-progressive.opt.jpg"
    , mkArray
        [ StartOfImage
        , Application 0 "JFIF\x01\SOH\SOH\SOH\SOH,\SOH,\x01\x01" ""
        , DefineQuantizationTable "\x01\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255"
        , DefineQuantizationTable "\SOH\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255"
        , StartOfFrame 0 "\b\x01\b\x01\b\ETX\SOH\DC1\x01\STX\DC1\SOH\ETX\DC1\SOH"
        , DefineHuffmanTable "\x01\SOH\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\STX"
        , DefineHuffmanTable "\DLE\SOH\SOH\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\240\225"
        , DefineHuffmanTable "\SOH\SOH\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\SOH"
        , DefineHuffmanTable "\DC1\SOH\SOH\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\240\225"
        , StartOfScan "\ETX\SOH\x01\STX\DC1\ETX\DC1\x01?\x01" "b\STX\STX\DEL"
        , EndOfImage
        ]
    )
  ]

spec :: Spec
spec = do
  describe "jpegP" $
    forM_ jpegExamples $ \(jpegFile, jpeg) -> do
      jpegData <- runIO $ BS.readFile ("test/Jpeg/" ++ jpegFile)
      itWith "should work with " jpegP (jpegData, jpeg)

  describe "encode" $
    forM_ jpegExamples $ \(jpegFile, _jpeg) -> do
      jpegData <- runIO $ BS.readFile ("test/Jpeg/" ++ jpegFile)
      let jpegParsed = case parseDetail jpegP jpegData of
            Left err            -> error $ "Failed to parse JPEG: " ++ show err
            Right (_, _, jpeg') -> jpeg'
      it ("decodes/encodes without modification" ++ jpegFile) $ do
        encode jpegParsed `shouldBe` jpegData
