{-|
This modules implements optimization techniques targeted at XML strings.
-}
module Codec.Compression.XML
  ( optimizeXML
  ) where

import qualified Data.ByteString               as BS
import qualified Data.ByteString.UTF8          as BSU
import           Text.XML.Light                 ( showContent
                                                , parseXML
                                                , Content(Elem, Text)
                                                , Element(Element, elContent)
                                                , CData(CData)
                                                )
import           Data.Char                      ( isSpace )

{- | Optimize XML stream.

It:

- removes indentation spaces
- transforms empty tag <a></a> into <a />
-}
optimizeXML :: BS.ByteString -> BS.ByteString
optimizeXML stream = BS.concat
  (BSU.fromString . showContent <$> removeSpace (parseXML stream))
 where
  isIndent :: Content -> Bool
  isIndent (Text (CData _ ('\n' : remain) _)) = all isSpace remain
  isIndent _ = False

  removeSpace :: [Content] -> [Content]
  removeSpace (text@(Text _) : remain) | isIndent text = removeSpace remain
                                       | otherwise = text : removeSpace remain
  removeSpace (Elem element@(Element _ _ content _) : remain) =
    Elem element { elContent = removeSpace content } : removeSpace remain
  removeSpace (other : remain) = other : removeSpace remain
  removeSpace []               = []
