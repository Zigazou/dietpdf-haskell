{-# LANGUAGE OverloadedStrings #-}
module Pdf.Object.OptimizeSpec
  ( spec
  ) where

import           Control.Monad                  ( forM_ )
import qualified Data.ByteString.Lazy          as BL
import qualified Data.HashMap.Strict           as HM
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFIndirectObject
                                                  , PDFDictionary
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
import           Data.Either.Extra              ( eitherToMaybe )
import           Util.Step                      ( runExceptT )


objectExamples :: [(PDFObject, PDFObject)]
objectExamples =
  [ ( PDFIndirectObject
      1
      0
      (PDFDictionary
        (HM.fromList
          [("Size", PDFNumber 16.0), ("Filter", PDFName "FlateDecode")]
        )
      )
      (Just . BL.toStrict . ZL.compress . BL.fromStrict $ "Hello, world!")
    , PDFIndirectObject
      1
      0
      (PDFDictionary
        (HM.fromList
          [("Size", PDFNumber 16.0), ("Filter", PDFName "RunLengthDecode")]
        )
      )
      (eitherToMaybe $ RL.compress "Hello, world!")
    )
  ]

spec :: Spec
spec = describe "optimize" $ forM_ objectExamples $ \(example, expected) ->
  it ("should be optimized " ++ show example)
    $ do
      optimized <- runExceptT (optimize example)
      optimized `shouldBe` Right expected
