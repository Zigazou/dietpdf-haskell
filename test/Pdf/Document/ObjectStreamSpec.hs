module Pdf.Document.ObjectStreamSpec
  ( spec
  ) where

import Control.Monad (forM_)
import Control.Monad.Trans.Except (runExceptT)

import Data.Array (mkArray)
import Data.UnifiedError (Fallible, UnifiedError (NoObjectToEncode))

import Pdf.Document.Document (PDFDocument, fromList)
import Pdf.Document.ObjectStream (explodeList, insert)
import Pdf.Object.Object
    ( PDFObject (PDFArray, PDFDictionary, PDFHexString, PDFIndirectObject, PDFIndirectObjectWithStream, PDFName, PDFNull, PDFNumber, PDFObjectStream, PDFString)
    , mkPDFArray
    , mkPDFDictionary
    )

import Test.Hspec (Spec, describe, it, shouldBe)

import Util.Dictionary (mkDictionary, mkEmptyDictionary)

fromObjectStreamExamples :: [(PDFObject, Fallible [PDFObject])]
fromObjectStreamExamples =
  [ ( PDFObjectStream
      1
      0
      (mkDictionary
        [ ("Type" , PDFName "ObjStm")
        , ("N"    , PDFNumber 2)
        , ("First", PDFNumber 10)
        ]
      )
      "24 0 18 3 10 (Hello)"
    , Right
      [ PDFIndirectObject 24 0 (PDFNumber 10.0)
      , PDFIndirectObject 18 0 (PDFString "Hello")
      ]
    )
  , (PDFNull, Right [PDFNull])
  , ( PDFObjectStream
      1
      0
      (mkDictionary
        [ ("Type" , PDFName "ObjStm")
        , ("N"    , PDFNumber 2)
        , ("First", PDFNumber 10)
        ]
      )
      "24 0 18 3 10  (Hello)"
    , Right
      [ PDFIndirectObject 24 0 (PDFNumber 10.0)
      , PDFIndirectObject 18 0 (PDFString "Hello")
      ]
    )
  , ( PDFObjectStream
      1
      0
      (mkDictionary
        [ ("Type" , PDFName "ObjStm")
        , ("N"    , PDFNumber 2)
        , ("First", PDFNumber 10)
        ]
      )
      "24 0 18 3 10 (Hello) "
    , Right
      [ PDFIndirectObject 24 0 (PDFNumber 10.0)
      , PDFIndirectObject 18 0 (PDFString "Hello")
      ]
    )
  , ( PDFObjectStream
      1
      0
      (mkDictionary
        [ ("Type" , PDFName "ObjStm")
        , ("N"    , PDFNumber 3)
        , ("First", PDFNumber 20)
        ]
      )
      "92 0 94 283 95 494\n\
      \\n\
      \<<\n\
      \/Type /Annot\n\
      \/F 4\n\
      \/Subtype /Link\n\
      \/Border [0 0 0]\n\
      \/Rect [264.976 234.538 516.905 254.607]\n\
      \/Contents <FEFF00680074007400700073003A002F002F006700690074006800750062002E0063006F006D002F007A006900670061007A006F0075>\n\
      \/A <<\n\
      \/Type /Action\n\
      \/S /URI\n\
      \/URI (https://github.com/zigazou)\n\
      \>>\n\
      \>>\n\
      \\n\
      \\n\
      \<<\n\
      \/Type /Annot\n\
      \/F 4\n\
      \/Subtype /Link\n\
      \/Border [0 0 0]\n\
      \/Rect [264.607 198.821 351.588 218.89]\n\
      \/Contents <FEFF0040007A006900670061007A006F0075>\n\
      \/A <<\n\
      \/Type /Action\n\
      \/S /URI\n\
      \/URI (https://twitter.com/zigazou)\n\
      \>>\n\
      \>>\n\
      \\n\
      \\n\
      \<<\n\
      \/Type /Annot\n\
      \/F 4\n\
      \/Subtype /Link\n\
      \/Border [0 0 0]\n\
      \/Rect [263.984 163.105 502.562 183.174]\n\
      \/Contents <FEFF007A006900670061007A006F0075004000700072006F0074006F006E006D00610069006C002E0063006F006D>\n\
      \/A <<\n\
      \/Type /Action\n\
      \/S /URI\n\
      \/URI (mailto:zigazou@protonmail.com)\n\
      \>>\n\
      \>>\n"
    , Right
      [ PDFIndirectObject 92 0 $ mkPDFDictionary
        [ ("Type"   , PDFName "Annot")
        , ("F"      , PDFNumber 4)
        , ("Subtype", PDFName "Link")
        , ("Border", PDFArray $ mkArray [PDFNumber 0, PDFNumber 0, PDFNumber 0])
        , ( "Rect"
          , PDFArray $ mkArray
            [ PDFNumber 264.976
            , PDFNumber 234.538
            , PDFNumber 516.905
            , PDFNumber 254.607
            ]
          )
        , ( "Contents"
          , PDFHexString
            "feff00680074007400700073003a002f002f006700690074006800750062002e0063006f006d002f007a006900670061007a006f0075"
          )
        , ( "A"
          , PDFDictionary $ mkDictionary
            [ ("Type", PDFName "Action")
            , ("S"   , PDFName "URI")
            , ("URI" , PDFString "https://github.com/zigazou")
            ]
          )
        ]
      , PDFIndirectObject 94 0 $ mkPDFDictionary
        [ ( "A"
          , mkPDFDictionary
            [ ("S"   , PDFName "URI")
            , ("Type", PDFName "Action")
            , ("URI" , PDFString "https://twitter.com/zigazou")
            ]
          )
        , ("Border", mkPDFArray [PDFNumber 0.0, PDFNumber 0.0, PDFNumber 0.0])
        , ("Contents", PDFHexString "feff0040007a006900670061007a006f0075")
        , ("F"       , PDFNumber 4.0)
        , ( "Rect"
          , mkPDFArray
            [ PDFNumber 264.607
            , PDFNumber 198.821
            , PDFNumber 351.588
            , PDFNumber 218.89
            ]
          )
        , ("Subtype", PDFName "Link")
        , ("Type"   , PDFName "Annot")
        ]
      , PDFIndirectObject 95 0 $ mkPDFDictionary
        [ ("Type"   , PDFName "Annot")
        , ("F"      , PDFNumber 4)
        , ("Subtype", PDFName "Link")
        , ("Border", PDFArray $ mkArray [PDFNumber 0, PDFNumber 0, PDFNumber 0])
        , ( "Rect"
          , PDFArray $ mkArray
            [ PDFNumber 263.984
            , PDFNumber 163.105
            , PDFNumber 502.562
            , PDFNumber 183.174
            ]
          )
        , ( "Contents"
          , PDFHexString
            "feff007a006900670061007a006f0075004000700072006f0074006f006e006d00610069006c002e0063006f006d"
          )
        , ( "A"
          , PDFDictionary $ mkDictionary
            [ ("Type", PDFName "Action")
            , ("S"   , PDFName "URI")
            , ("URI" , PDFString "mailto:zigazou@protonmail.com")
            ]
          )
        ]
      ]
    )
  ]

toObjectStreamExamples :: [(PDFDocument, Fallible PDFObject)]
toObjectStreamExamples =
  [ ( fromList
      [ PDFIndirectObject 24 0 (PDFNumber 10.0)
      , PDFIndirectObject 18 0 (PDFString "Hello")
      ]
    , Right $ PDFObjectStream
      1
      0
      (mkDictionary
        [ ("Type" , PDFName "ObjStm")
        , ("N"    , PDFNumber 2)
        , ("First", PDFNumber 10)
        ]
      )
      "24 0 18 3 10 (Hello)"
    )
  , (fromList []       , Left NoObjectToEncode)
  , (fromList [PDFNull], Left NoObjectToEncode)
  , ( fromList
      [PDFIndirectObjectWithStream 1 0 mkEmptyDictionary "Hello, World!"]
    , Left NoObjectToEncode
    )
  ]

spec :: Spec
spec = do
  describe "extract" $ do
    forM_ fromObjectStreamExamples $ \(example, expected) ->
      it ("should decode " ++ show example)
        $   runExceptT (explodeList [example])
        >>= (`shouldBe` expected)

  describe "insert" $ do
    forM_ toObjectStreamExamples $ \(example, expected) ->
      it ("should encode " ++ show example)
        $   runExceptT (insert example 1)
        >>= (`shouldBe` expected)
