module Font.TrueType.Parser.HeadSpec
  ( spec
  ) where

import           Test.Hspec                     ( describe
                                                , Spec
                                                , shouldBe
                                                , it
                                                )
import           Data.Binary.Parser             ( parseOnly )
import qualified Data.ByteString               as BS
import           Font.TrueType.Parser.Head      ( headP )
import           Font.TrueType.FontTable        ( Head
                                                  ( Head
                                                  , hVersion
                                                  , hFontRevision
                                                  , hCheckSumAdjustment
                                                  , hMagicNumber
                                                  , hFlags
                                                  , hUnitsPerEm
                                                  , hCreated
                                                  , hModified
                                                  , hXMin
                                                  , hYMin
                                                  , hXMax
                                                  , hYMax
                                                  , hMacStyle
                                                  , hLowestRecPPEM
                                                  , hFontDirectionHint
                                                  , hIndexToLocFormat
                                                  , hGlyphDataFormat
                                                  )
                                                , Fixed(Fixed)
                                                )
import           Control.Monad                  ( forM_ )

headPExamples :: [(BS.ByteString, Head)]
headPExamples =
  [ ( "\x00\x01\x00\x00\x00\x02\x23\x12\xA9\xDE\xA8\x42\x5F\x0F\x3C\xF5\x00\
      \\x19\x08\x00\x00\x00\x00\x00\xC4\xF0\x11\x2E\x00\x00\x00\x00\xD5\x01\
      \\x52\xF4\xFA\x1B\xFD\xD5\x09\x30\x08\x73\x00\x00\x00\x09\x00\x02\x00\
      \\x00\x00\x00"
    , Head { hVersion            = Fixed 1 0
           , hFontRevision       = Fixed 2 0x2312
           , hCheckSumAdjustment = 0xa9dea842
           , hMagicNumber        = 0x5f0f3cf5
           , hFlags              = 0x19
           , hUnitsPerEm         = 2048
           , hCreated            = 0x00000000C4F0112E
           , hModified           = 0x00000000D50152F4
           , hXMin               = -1509
           , hYMin               = -555
           , hXMax               = 2352
           , hYMax               = 2163
           , hMacStyle           = 0
           , hLowestRecPPEM      = 9
           , hFontDirectionHint  = 2
           , hIndexToLocFormat   = 0
           , hGlyphDataFormat    = 0
           }
    )
  ]

spec :: Spec
spec = do
  describe "headP" $ do
    forM_ headPExamples $ \(source, expected) ->
      it ("decodes " ++ show source) $ do
        parseOnly headP source `shouldBe` Right expected
