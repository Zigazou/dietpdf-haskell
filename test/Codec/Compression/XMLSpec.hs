{-# LANGUAGE OverloadedStrings #-}
module Codec.Compression.XMLSpec
  ( spec
  ) where

import           Control.Monad                  ( forM_ )
import qualified Data.ByteString               as BS
import           Codec.Compression.XML          ( optimizeXML )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )

xmlExamples :: [(BS.ByteString, BS.ByteString)]
xmlExamples =
  [ ( "<?xml version=\"1.0\" ?>\n\
      \<a>aa</a>\n"
    , "<?xml version=\"1.0\" ?><a>aa</a>"
    )
  , ( "<?xml version=\"1.0\" ?>\n\
      \    <a>  </a>\n"
    , "<?xml version=\"1.0\" ?><a>  </a>"
    )
  , ( "<?xml version=\"1.0\" ?>\n\
      \<a>\n\
      \  <b>\n\
      \    <c>cc</c>\n\
      \  </b>\n\
      \</a>\n"
    , "<?xml version=\"1.0\" ?><a><b><c>cc</c></b></a>"
    )
  , ( "<?xml version=\"1.0\" ?>\n\
      \             \n\
      \       \t    \n\
      \    <a>  </a>\n"
    , "<?xml version=\"1.0\" ?><a>  </a>"
    )
  ]

spec :: Spec
spec = describe "optimizeXML" $ do
  forM_ xmlExamples $ \(example, expected) ->
    it ("should work with " ++ show example)
      $          optimizeXML example
      `shouldBe` expected
