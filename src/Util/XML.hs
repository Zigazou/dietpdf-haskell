{-|
Small XML helpers for prefix renaming.

This module contains a few helpers for working with XML documents represented
using "Text.XML.Light".

The main feature is renaming XML namespace prefixes consistently across an
entire document tree, using a 'Data.TranslationTable.TranslationTable' to map
old prefixes to new ones.
-}
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

{-|
Rename an XML 'QName' using a prefix translation table.
-}
renamePrefixInQName :: TranslationTable String -> QName -> QName
renamePrefixInQName toNewPrefixes qname@(QName name _uri (Just "xmlns")) =
  qname { qName = convert toNewPrefixes name }
renamePrefixInQName toNewPrefixes qname@(QName _name _uri prefix) =
  qname { qPrefix = convert toNewPrefixes <$> prefix }

{-|
Rename an XML 'Attr' using a prefix translation table.
-}
renamePrefixInAttr :: TranslationTable String -> Attr -> Attr
renamePrefixInAttr toNewPrefixes (Attr qname value) =
  Attr (renamePrefixInQName toNewPrefixes qname) value

{-|
Rename prefixes in an XML 'Content' using a prefix translation table.
-}
renamePrefixInContent :: TranslationTable String -> Content -> Content
renamePrefixInContent toNewPrefixes (Elem e) =
  Elem (renamePrefixesInElement toNewPrefixes e)
renamePrefixInContent _toNewPrefixes c = c

{-|
Rename prefixes in an XML 'Element' subtree.

This updates:

- the element name;
- all attribute names; and
- all nested elements.

Prefix renaming is performed via the provided translation table.
- Special case: attributes in the @xmlns@ and @xml@ namespaces are treated as
  namespace declarations and their /name/ is translated.
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

{-|
Collect all namespace prefixes referenced by an XML 'QName'.
-}
getAllPrefixesInQName :: QName -> [String]
getAllPrefixesInQName (QName name _uri (Just "xmlns")) = [name]
getAllPrefixesInQName (QName name _uri (Just "xml"  )) = [name]
getAllPrefixesInQName (QName _name _uri prefix)        = maybe [] pure prefix

{-|
Collect all namespace prefixes referenced by an XML 'Attr'.
-}
getAllPrefixesInAttr :: Attr -> [String]
getAllPrefixesInAttr (Attr qname _value) = getAllPrefixesInQName qname

{-|
Collect all namespace prefixes referenced by an XML 'Content'.
-}
getAllPrefixesInContent :: Content -> [String]
getAllPrefixesInContent (Elem e) = getAllPrefixesInElement e
getAllPrefixesInContent _        = []

{-|
Collect all namespace prefixes referenced by an XML 'Element'.
-}
getAllPrefixesInElement :: Element -> [String]
getAllPrefixesInElement element =
  getAllPrefixesInQName (elName element)
  <> concatMap getAllPrefixesInAttr (elAttribs element)
  <> concatMap getAllPrefixesInContent (elContent element)

{-|
Collect all namespace prefixes referenced by a list of XML contents.

The returned list may contain duplicates.

The reserved prefixes @xmlns@ and @xml@ are excluded from the result.
-}
getAllPrefixes :: [Content] -> [String]
getAllPrefixes contents = concatMap (filter (`notElem` ["xmlns", "xml"])) $ do
  contents >>= \case
    Elem element  -> return $ getAllPrefixesInElement element
    _otherContent -> return []

{-|
Alphabet used by 'toNameBase'.

This is a simple base-26 digit table using lowercase ASCII letters.
-}
baseDigits :: ByteString
baseDigits = "abcdefghijklmnopqrstuvwxyz"

{-|
Generate a short, deterministic prefix name from an integer.

The result uses a base-26 representation over lowercase ASCII letters. This is
useful for generating compact namespace prefixes (e.g. @a@, @b@, @aa@).

The first argument is currently ignored; it exists to preserve the call shape
used by other prefix-generation helpers in the codebase.
-}
toNameBase :: String -> Int -> String
toNameBase _orig value = toNameBase' value ""
  where
    toNameBase' :: Int -> String -> String
    toNameBase' 0 ""  = "a"
    toNameBase' 0 acc = acc
    toNameBase' n acc =
      let (quotient, remainder) = n `divMod` BS.length baseDigits
      in toNameBase' quotient (w2c (BS.index baseDigits remainder):acc)

{-|
Replace prefixes across a list of XML contents.

This is the top-level helper that applies prefix renaming to each 'Element'
encountered in the input, leaving non-element content unchanged.

See 'renamePrefixesInElement' for details about how names are rewritten.
-}
renamePrefixes :: TranslationTable String -> [Content] -> [Content]
renamePrefixes toNewPrefixes contents = concat $ do
  contents >>= \case
    Elem element -> return [Elem (renamePrefixesInElement toNewPrefixes element)]
    otherContent -> return [otherContent]
