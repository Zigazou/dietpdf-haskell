module Util.XML
  ( renamePrefixes
  , toNameBase
  , getAllPrefixes
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal (w2c)
import Data.TranslationTable (TranslationTable, convert)

import Text.XML.Light
    ( Attr (Attr)
    , Content (Elem)
    , Element (elAttribs, elContent, elName)
    , QName (QName, qName, qPrefix)
    )

renamePrefixInQName :: TranslationTable String -> QName -> QName
renamePrefixInQName toNewPrefixes qname@(QName name _uri (Just "xmlns")) =
  qname { qName = convert toNewPrefixes name }
renamePrefixInQName toNewPrefixes qname@(QName _name _uri prefix) =
  qname { qPrefix = convert toNewPrefixes <$> prefix }

renamePrefixInAttr :: TranslationTable String -> Attr -> Attr
renamePrefixInAttr toNewPrefixes (Attr qname value) =
  Attr (renamePrefixInQName toNewPrefixes qname) value

renamePrefixInContent :: TranslationTable String -> Content -> Content
renamePrefixInContent toNewPrefixes (Elem e) =
  Elem (renamePrefixesInElement toNewPrefixes e)
renamePrefixInContent _toNewPrefixes c = c

{- |
Replace the prefixes in an XML document.
-}
renamePrefixesInElement
  :: TranslationTable String
  -> Element
  -> Element
renamePrefixesInElement toNewPrefixes element = element
  { elName    = renamePrefixInQName   toNewPrefixes (elName element)
  , elAttribs = renamePrefixInAttr    toNewPrefixes <$> elAttribs element
  , elContent = renamePrefixInContent toNewPrefixes <$> elContent element
  }

getAllPrefixesInQName :: QName -> [String]
getAllPrefixesInQName (QName name _uri (Just "xmlns")) = [name]
getAllPrefixesInQName (QName _name _uri prefix)        = maybe [] pure prefix

getAllPrefixesInAttr :: Attr -> [String]
getAllPrefixesInAttr (Attr qname _value) = getAllPrefixesInQName qname

getAllPrefixesInContent :: Content -> [String]
getAllPrefixesInContent (Elem e) = getAllPrefixesInElement e
getAllPrefixesInContent _        = []

getAllPrefixesInElement :: Element -> [String]
getAllPrefixesInElement element =
  getAllPrefixesInQName (elName element)
  <> concatMap getAllPrefixesInAttr (elAttribs element)
  <> concatMap getAllPrefixesInContent (elContent element)

getAllPrefixes :: [Content] -> [String]
getAllPrefixes contents = concatMap (filter (/= "xmlns")) $ do
  contents >>= \case
    Elem element  -> return $ getAllPrefixesInElement element
    _otherContent -> return []

baseDigits :: ByteString
baseDigits = "abcdefghijklmnopqrstuvwxyz"

toNameBase :: String -> Int -> String
toNameBase _orig value = toNameBase' value ""
  where
    toNameBase' :: Int -> String -> String
    toNameBase' 0 ""  = "a"
    toNameBase' 0 acc = acc
    toNameBase' n acc =
      let (quotient, remainder) = n `divMod` BS.length baseDigits
      in toNameBase' quotient (w2c (BS.index baseDigits remainder):acc)

{- |
Replace the prefixes in an XML document.
-}
renamePrefixes :: TranslationTable String -> [Content] -> [Content]
renamePrefixes toNewPrefixes contents = concat $ do
  contents >>= \case
    Elem element -> return [Elem (renamePrefixesInElement toNewPrefixes element)]
    otherContent -> return [otherContent]
