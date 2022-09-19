{-# LANGUAGE OverloadedStrings #-}
module Codec.Compression.RunLengthSpec
  ( spec
  ) where

import           Control.Monad                  ( forM_
                                                , forM
                                                )
import qualified Data.ByteString               as BS
import           Data.Word                      ( Word8 )
import           Codec.Compression.RunLength    ( compress
                                                , decompress
                                                )
import           Util.Errors                    ( UnifiedError )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )
import           Test.QuickCheck                ( Gen
                                                , arbitrary
                                                , forAll
                                                )

decodeExamples :: [(BS.ByteString, Either UnifiedError BS.ByteString)]
decodeExamples =
  [ (""                        , Right "")
  , ("\o365a"                  , Right "aaaaaaaaaaaa")
  , ("\o365a\o365b"            , Right "aaaaaaaaaaaabbbbbbbbbbbb")
  , ("\o365a\o365b\o000c\o000d", Right "aaaaaaaaaaaabbbbbbbbbbbbcd")
  ]

encodeExamples :: [(BS.ByteString, Either UnifiedError BS.ByteString)]
encodeExamples =
  [ (""                          , Right "")
  , ("a"                         , Right "\o000a")
  , ("aaaaaaaaaaaa"              , Right "\o365a")
  , ("aaaaaaaaaaaabbbbbbbbbbbb"  , Right "\o365a\o365b")
  , ("aaaaaaaaaaaabbbbbbbbbbbbcd", Right "\o365a\o365b\o001cd")
  ]

randomString :: Gen BS.ByteString
randomString = do
  items         <- arbitrary :: Gen [Word8]
  expandedItems <- forM items $ \item -> do
    replicateCount <- arbitrary
    return $ replicate replicateCount item
  return $ BS.pack (concat expandedItems)

spec :: Spec
spec = describe "runLength" $ do
  forM_ decodeExamples $ \(example, expected) ->
    it ("should decode bytestring " ++ show example)
      $          decompress example
      `shouldBe` expected

  forM_ encodeExamples $ \(example, expected) ->
    it ("should encode bytestring " ++ show example)
      $          compress example
      `shouldBe` expected

  it "encode then decode should give the same value"
    $ forAll randomString
    $ \x -> (compress x >>= decompress) `shouldBe` Right x
