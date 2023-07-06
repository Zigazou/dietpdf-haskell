{-# LANGUAGE OverloadedStrings #-}
module Pdf.Object.OptimizeSpec
  ( spec
  ) where

import           Control.Monad                  ( forM_ )
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Map.Strict               as Map
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFIndirectObjectWithStream
                                                  , PDFNumber
                                                  , PDFName
                                                  )
                                                )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )
import qualified Codec.Compression.Zlib        as ZL
import qualified Codec.Compression.RunLength   as RL
import           Pdf.Object.Optimize            ( optimize )
import           Data.Either.Extra              ( fromRight )
import           Control.Monad.Trans.Except     ( runExceptT )


objectExamples :: [(PDFObject, PDFObject)]
objectExamples =
  [ ( PDFIndirectObjectWithStream
      1
      0
      (Map.fromList
        [("Size", PDFNumber 16.0), ("Filter", PDFName "FlateDecode")]
      )
      (BL.toStrict . ZL.compress . BL.fromStrict $ "Hello, world!")
    , PDFIndirectObjectWithStream
      1
      0
      (Map.fromList
        [("Size", PDFNumber 16.0), ("Filter", PDFName "RunLengthDecode")]
      )
      (fromRight "" $ RL.compress "Hello, world!")
    )
  ]

spec :: Spec
spec = describe "optimize" $ forM_ objectExamples $ \(example, expected) ->
  it ("should be optimized " ++ show example) $ do
    optimized <- runExceptT (optimize example)
    optimized `shouldBe` Right expected
