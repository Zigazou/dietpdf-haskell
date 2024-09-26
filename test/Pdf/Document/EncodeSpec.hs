module Pdf.Document.EncodeSpec
  ( spec
  ) where

import Control.Monad (forM_)
import Control.Monad.Trans.Except (runExceptT)

import Data.Sequence (Seq (Empty), fromList)
import Data.UnifiedError (Fallible)

import Pdf.Document.Encode (encodeObject)
import Pdf.Document.EncodedObject (EncodedObject (EncodedObject))
import Pdf.Object.Object
    ( PDFObject (PDFIndirectObject, PDFIndirectObjectWithStream, PDFName, PDFObjectStream, PDFString)
    )
import Pdf.Object.Object.PDFObject (PDFObject (PDFNumber))

import Test.Hspec (Spec, describe, it, shouldBe)

import Util.Dictionary (mkDictionary, mkEmptyDictionary)

encodeObjectExamples :: [(PDFObject, Fallible EncodedObject)]
encodeObjectExamples =
  [ ( PDFIndirectObject 1 0 (PDFString "Hello, World!")
    , Right $ EncodedObject 1 13 "Hello, World!" Empty
    )
  , ( PDFIndirectObjectWithStream 1 0 mkEmptyDictionary "Hello, World!"
    , Right $ EncodedObject 1 13 "Hello, World!" Empty
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
    , Right $ EncodedObject 1 843
        "1 0 obj<</First 20/N 3/Type/ObjStm>>stream\n92 0 94 283 95 494\n\n<<\n\
        \/Type /Annot\n/F 4\n/Subtype /Link\n/Border [0 0 0]\n/Rect [264.976 \
        \234.538 516.905 254.607]\n/Contents <FEFF00680074007400700073003A002F\
        \002F006700690074006800750062002E0063006F006D002F007A006900670061007A\
        \006F0075>\n/A <<\n/Type /Action\n/S /URI\n/URI (https://github.com/\
        \zigazou)\n>>\n>>\n\n\n<<\n/Type /Annot\n/F 4\n/Subtype /Link\n/Border \
        \[0 0 0]\n/Rect [264.607 198.821 351.588 218.89]\n/Contents \
        \<FEFF0040007A006900670061007A006F0075>\n/A <<\n/Type /Action\n/S \
        \/URI\n/URI (https://twitter.com/zigazou)\n>>\n>>\n\n\n<<\n/Type /Annot\
        \\n/F 4\n/Subtype /Link\n/Border [0 0 0]\n/Rect [263.984 163.105 \
        \502.562 183.174]\n/Contents <FEFF007A006900670061007A006F00750040007\
        \00072006F0074006F006E006D00610069006C002E0063006F006D>\n/A <<\n/Type \
        \/Action\n/S /URI\n/URI (mailto:zigazou@protonmail.com)\n>>\n>>\n\n\
        \endstream endobj\n"
        (fromList [92, 94, 95])
    )
  ]

spec :: Spec
spec = do
  describe "encodeObject" $ do
    forM_ encodeObjectExamples $ \(example, expected) ->
      it ("should encode object " ++ show example)
        $   runExceptT (encodeObject example)
        >>= (`shouldBe` expected)
