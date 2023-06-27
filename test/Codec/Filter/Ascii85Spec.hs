{-# LANGUAGE OverloadedStrings #-}
module Codec.Filter.Ascii85Spec
  ( spec
  ) where

import           Codec.Filter.Ascii85           ( decode
                                                , encode
                                                )
import           Control.Monad                  ( forM
                                                , forM_
                                                )
import qualified Data.ByteString               as BS
import           Data.Word                      ( Word8 )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )
import           Test.QuickCheck                ( Gen
                                                , arbitrary
                                                , forAll
                                                )
import           Util.Errors                    ( UnifiedError )

decodeExamples :: [(BS.ByteString, Either UnifiedError BS.ByteString)]
decodeExamples =
  [ ("87cURD_*#4DfTZ)+T~>"    , Right "Hello, World!")
  , ("87cUR D_*#4 DfTZ) +T ~>", Right "Hello, World!")
  , ("z~>"                    , Right "\x00\x00\x00\x00")
  , ("87cUR~>"                , Right "Hell")
  , ("87cURz~>"               , Right "Hell\x00\x00\x00\x00")
  , ("5sdq,77Kd<~>"           , Right "ABCDEFGH")
  , ("5sdq,77Kd<8H~>"         , Right "ABCDEFGHI")
  , ("5sdq,77Kd<8P/~>"        , Right "ABCDEFGHIJ")
  , ("5sdq,77Kd<8P2V~>"       , Right "ABCDEFGHIJK")
  ]

encodeExamples :: [(BS.ByteString, Either UnifiedError BS.ByteString)]
encodeExamples =
  [ (""                , Right "~>")
  , ("\x00\x00\x00\x00", Right "z~>")
  , ("Hello, World!"   , Right "87cURD_*#4DfTZ)+T~>")
  , ("ABCDEFGH"        , Right "5sdq,77Kd<~>")
  , ("ABCDEFGHI"       , Right "5sdq,77Kd<8H~>")
  , ("ABCDEFGHIJ"      , Right "5sdq,77Kd<8P/~>")
  , ("ABCDEFGHIJK"     , Right "5sdq,77Kd<8P2V~>")
  ]

randomString :: Gen BS.ByteString
randomString = do
  items         <- arbitrary :: Gen [Word8]
  expandedItems <- forM items $ \item -> do
    replicateCount <- arbitrary
    return $ replicate replicateCount item
  return $ BS.pack (concat expandedItems)

spec :: Spec
spec = do
  describe "decode" $ forM_ decodeExamples $ \(example, expected) ->
    it ("should decode bytestring " ++ show example)
      $          decode example
      `shouldBe` expected

  describe "encode" $ forM_ encodeExamples $ \(example, expected) ->
    it ("should encode bytestring " ++ show example)
      $          encode example
      `shouldBe` expected

  describe "random values"
    $ it "encode then decode should give the same value"
    $ forAll randomString
    $ \x -> (encode x >>= decode) `shouldBe` Right x
