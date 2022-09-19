{-# LANGUAGE OverloadedStrings #-}
module Pdf.Object.LinearizationSpec
  ( spec
  ) where

import           Test.Hspec                     ( describe
                                                , Spec
                                                , shouldBe
                                                , it
                                                )
import           Control.Monad                  ( forM_ )
import qualified Data.ByteString               as BS
import           Pdf.Parser.Parser              ( pdfParse )
import           Pdf.Object.Object              ( PDFObject )
import           Pdf.Object.Linearization       ( Linearization
                                                  ( Linearization
                                                  , lnVersion
                                                  , lnFileLength
                                                  , lnPrimaryHintOffset
                                                  , lnPrimaryHintLength
                                                  , lnOverflowHintOffset
                                                  , lnOverflowHintLength
                                                  , lnFirstPageObjectNumber
                                                  , lnFirstPageEndOffset
                                                  , lnNumberOfPages
                                                  , lnXRefFirstEntryOffset
                                                  , lnFirstPageNumber
                                                  )
                                                , getLinearization
                                                )

linearizationExamples :: [(BS.ByteString, Maybe Linearization)]
linearizationExamples =
  [ ( "43 0 obj\n\
      \<< /Linearized 1.0\n\
      \/L 54567\n\
      \/H [ 475 598 ]\n\
      \/O 45\n\
      \/E 5437\n\
      \/N 11\n\
      \/T 52786\n\
      \>>\n\
      \endobj"
    , Just $ Linearization { lnVersion               = 1.0
                           , lnFileLength            = 54567
                           , lnPrimaryHintOffset     = 475
                           , lnPrimaryHintLength     = 598
                           , lnOverflowHintOffset    = Nothing
                           , lnOverflowHintLength    = Nothing
                           , lnFirstPageObjectNumber = 45
                           , lnFirstPageEndOffset    = 5437
                           , lnNumberOfPages         = 11
                           , lnXRefFirstEntryOffset  = 52786
                           , lnFirstPageNumber       = Nothing
                           }
    )
  , ( "43 0 obj\n\
      \<< /Linearized 1.0\n\
      \/L 54567\n\
      \/H [ 475 598 228 312 ]\n\
      \/O 45\n\
      \/E 5437\n\
      \/N 11\n\
      \/T 52786\n\
      \/P 24\n\
      \>>\n\
      \endobj"
    , Just $ Linearization { lnVersion               = 1.0
                           , lnFileLength            = 54567
                           , lnPrimaryHintOffset     = 475
                           , lnPrimaryHintLength     = 598
                           , lnOverflowHintOffset    = Just 228
                           , lnOverflowHintLength    = Just 312
                           , lnFirstPageObjectNumber = 45
                           , lnFirstPageEndOffset    = 5437
                           , lnNumberOfPages         = 11
                           , lnXRefFirstEntryOffset  = 52786
                           , lnFirstPageNumber       = Just 24
                           }
    )
  ]

linearizationBadExamples :: [(BS.ByteString, Maybe Linearization)]
linearizationBadExamples =
  [ ( "43 0 obj\n\
      \<< /Linearized 1.0\n\
      \/H [ 475 598 ]\n\
      \/O 45\n\
      \/E 5437\n\
      \/N 11\n\
      \/T 52786\n\
      \>>\n\
      \endobj"
    , Nothing
    )
  , ( "43 0 obj\n\
      \<< /Linearized 1.0\n\
      \/L /Error\n\
      \/H [ 475 598 ]\n\
      \/O 45\n\
      \/E 5437\n\
      \/N 11\n\
      \/T 52786\n\
      \>>\n\
      \endobj"
    , Nothing
    )
  ]

toObjects :: BS.ByteString -> [PDFObject]
toObjects stream = case pdfParse stream of
  Left  _       -> []
  Right objects -> objects

spec :: Spec
spec = describe "getLinearization" $ do
  forM_ linearizationExamples $ \(example, expected) -> do
    it ("should work with " ++ show example)
      $          (getLinearization . toObjects) example
      `shouldBe` expected

  forM_ linearizationBadExamples $ \(example, expected) -> do
    it ("should not work with " ++ show example)
      $          (getLinearization . toObjects) example
      `shouldBe` expected
