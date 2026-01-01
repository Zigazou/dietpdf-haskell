{-|
This modules implements optimization techniques targeted at XML strings.
-}
module Codec.Compression.XML
  ( optimizeXML
  ) where

import Control.Monad.State (lift)

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 (decode, encode)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Search (replace)
import Data.ByteString.UTF8 qualified as BSU
import Data.Char (isSpace)
import Data.Functor ((<&>))
import Data.Logging (Logging)
import Data.PDF.PDFWork (PDFWork)
import Data.Text (Text)
import Data.Text.Encoding (decodeLatin1, decodeUtf8')
import Data.TranslationTable (getTranslationTable)

import External.JpegTran (jpegtranOptimize)

import Text.XML.Light
    ( CData (CData)
    , CDataKind (CDataText)
    , Content (Elem, Text)
    , Element (Element, elContent)
    , QName (QName)
    , parseXML
    , showContent
    )

import Util.Number (round')
import Util.XML (getAllPrefixes, renamePrefixes, toNameBase)

toText :: ByteString -> Text
toText bytes = case decodeUtf8' bytes of
  Right text -> text
  Left  _    -> decodeLatin1 bytes

{-| Parse a number from a string.

>>> parseNumber "3.14"
Just 3.14

>>> parseNumber "3.14.15"
Nothing
-}
parseNumber :: String -> Maybe Double
parseNumber string = case reads string of
  [(number, "")] -> Just number
  _notANumber    -> Nothing

optimizePrefixes :: [Content] -> [Content]
optimizePrefixes contents =
  let prefixes = getAllPrefixes contents
      newPrefixes = getTranslationTable toNameBase prefixes
  in renamePrefixes newPrefixes contents

{-| Optimize XML stream.

It:

- removes indentation spaces
- transforms empty tag <a></a> into <a />
- optimizes embedded JPEG
- optimizes numbers
- rename prefixes
-}
optimizeXML
  :: Logging IO
  => ByteString
  -> PDFWork IO ByteString
optimizeXML xml = do
  let spaceRemoved = optimizePrefixes
                   . cleanNumbers
                   . removeSpace
                   . parseXML
                   . toText
                   $ xml

  imageOptimized <- optimizeImages spaceRemoved

  return $ ( BS.concat
           . fmap (patchByteOrder . BSU.fromString . showContent)
           ) imageOptimized
 where
  patchByteOrder :: ByteString -> ByteString
  patchByteOrder =
    toStrict . replace "&#65279;" ("\xEF\xBB\xBF" :: ByteString)

  isIndent :: Content -> Bool
  isIndent (Text (CData _ ('\n' : remain) _)) = all isSpace remain
  isIndent _                                  = False

  cleanNumbers :: [Content] -> [Content]
  cleanNumbers (content@(Text (CData kind string mline)):remain) =
    case parseNumber string of
      Just number -> Text (CData kind (show (round' 3 number)) mline)
                   : cleanNumbers remain
      Nothing     -> content : cleanNumbers remain
  cleanNumbers (Elem element@(Element _ _ content _) : remain) =
    Elem element { elContent = cleanNumbers content } : cleanNumbers remain
  cleanNumbers (other : remain) = other : cleanNumbers remain
  cleanNumbers []               = []

  optimizeImage :: String -> PDFWork IO String
  optimizeImage content =
    let content' = (toStrict . replace "&#xA;" ("" :: ByteString))
                 $ (toStrict . replace "\n" ("" :: ByteString))
                 $ (toStrict . replace "\r" ("" :: ByteString))
                   (BSU.fromString content)
    in case decode content' of
      Right image -> lift (jpegtranOptimize image) <&> BSU.toString . encode
      Left  _     -> return content

  optimizeImages :: [Content] -> PDFWork IO [Content]
  optimizeImages [] = return []
  optimizeImages ( Elem (Element (QName "image" uri prefix)
                                 attrs
                                 [Text (CData CDataText content cmline)]
                                 mline
                        )
                 : remain
                 ) = do
    optimizedImage <- optimizeImage content
    optimizedRemain <- optimizeImages remain
    return $ Elem (Element (QName "image" uri prefix)
                           attrs
                           [Text (CData CDataText optimizedImage cmline)]
                           mline
                  )
           : optimizedRemain

  optimizeImages (Elem element@(Element _ _ content _) : remain) = do
    contentOptimized <- optimizeImages content
    remainOptimized <- optimizeImages remain
    return $ Elem element { elContent = contentOptimized } : remainOptimized

  optimizeImages (item : remain) = do
    optimizedRemain <- optimizeImages remain
    return $ item : optimizedRemain

  removeSpace :: [Content] -> [Content]
  removeSpace (text@(Text _) : remain) | isIndent text = removeSpace remain
                                       | otherwise = text : removeSpace remain
  removeSpace (Elem element@(Element _ _ content _) : remain) =
    Elem element { elContent = removeSpace content } : removeSpace remain
  removeSpace (other : remain) = other : removeSpace remain
  removeSpace []               = []
