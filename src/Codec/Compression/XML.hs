{-|
This modules implements optimization techniques targeted at XML strings.
-}
module Codec.Compression.XML
  ( optimizeXML
  ) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Search.DFA (replace)
import Data.ByteString.UTF8 qualified as BSU
import Data.Char (isSpace)
import Data.Text qualified as T
import Data.Text.Encoding (decodeLatin1, decodeUtf8')

import Text.XML.Light
    ( CData (CData)
    , Content (Elem, Text)
    , Element (Element, elContent)
    , parseXML
    , showContent
    )

toText :: BS.ByteString -> T.Text
toText bytes = case decodeUtf8' bytes of
  Right text -> text
  Left  _    -> decodeLatin1 bytes
{- | Optimize XML stream.

It:

- removes indentation spaces
- transforms empty tag <a></a> into <a />
-}
optimizeXML :: BS.ByteString -> BS.ByteString
optimizeXML stream = BS.concat
  (   patchByteOrder
  .   BSU.fromString
  .   showContent
  <$> (removeSpace . parseXML . toText) stream
  )
 where
  patchByteOrder :: BS.ByteString -> BS.ByteString
  patchByteOrder =
    toStrict . replace "&#65279;" ("\xEF\xBB\xBF" :: BS.ByteString)

  isIndent :: Content -> Bool
  isIndent (Text (CData _ ('\n' : remain) _)) = all isSpace remain
  isIndent _                                  = False

  removeSpace :: [Content] -> [Content]
  removeSpace (text@(Text _) : remain) | isIndent text = removeSpace remain
                                       | otherwise = text : removeSpace remain
  removeSpace (Elem element@(Element _ _ content _) : remain) =
    Elem element { elContent = removeSpace content } : removeSpace remain
  removeSpace (other : remain) = other : removeSpace remain
  removeSpace []               = []
