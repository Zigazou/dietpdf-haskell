module Util.SimpleTiffSpec
  ( spec
  ) where

import Control.Monad.Except (runExceptT)

import Data.ByteString qualified as BS
import Data.ColorSpace (ColorSpace (ColorSpaceRGB, ColorSpaceGray))

import External.ExternalCommand (externalCommandBuf')

import Test.Hspec (Spec, describe, it, shouldBe)

import Util.SimpleTiff (simpleTiff)

rgbImage :: BS.ByteString
rgbImage = BS.pack
  [ 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF
  , 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF
  , 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF
  , 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF
  , 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF
  ]

pnmRgbImage :: BS.ByteString
pnmRgbImage = BS.concat ["P6\n", "4 9\n", "255\n", rgbImage]

grayImage :: BS.ByteString
grayImage = BS.pack
  [ 0x40, 0x80, 0x00, 0xFF
  , 0xFF, 0x00, 0xFF, 0x00
  , 0x40, 0x80, 0x00, 0xFF
  , 0xFF, 0x00, 0xFF, 0x00
  , 0x40, 0x80, 0x00, 0xFF
  , 0xFF, 0x00, 0xFF, 0x00
  , 0x40, 0x80, 0x00, 0xFF
  , 0xFF, 0x00, 0xFF, 0x00
  , 0x40, 0x80, 0x00, 0xFF
  ]

pnmGrayImage :: BS.ByteString
pnmGrayImage = BS.concat ["P5\n", "4 9\n", "255\n", grayImage]

{-
cmykImage :: BS.ByteString
cmykImage = BS.pack
  [ 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0x00
  , 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0x00
  , 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0x00
  , 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0x00
  , 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0x00
  ]

pnmCmykImage :: BS.ByteString
pnmCmykImage = BS.concat ["P6\n", "4 9\n", "255\n", cmykImage]
-}

spec :: Spec
spec = do
  describe "simpleTiff" $ do
    it "creates a simple RGB TIFF file" $ do
      let tiff = simpleTiff 4 9 ColorSpaceRGB rgbImage
      raw <- runExceptT $ externalCommandBuf' "tifftopnm" [ "-quiet"] tiff
      raw `shouldBe` Right pnmRgbImage

    it "creates a simple Grayscale TIFF file" $ do
      let tiff = simpleTiff 4 9 ColorSpaceGray grayImage
      raw <- runExceptT $ externalCommandBuf' "tifftopnm" [ "-quiet"] tiff
      raw `shouldBe` Right pnmGrayImage

    {-
    it "creates a simple CMYK TIFF file" $ do
      let tiff = simpleTiff 4 9 4 cmykImage
      raw <- runExceptT $ externalCommandBuf' "tifftopnm" [ "-quiet"] tiff
      raw `shouldBe` Right pnmCmykImage
    -}
