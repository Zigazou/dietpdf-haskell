module Util.XMLSpec
  ( spec
  ) where

import Control.Monad (forM_)

import Data.ByteString qualified as BS
import Data.ByteString.UTF8 qualified as BSU
import Data.List (sort)
import Data.List.Extra (nub)
import Data.TranslationTable (getTranslationTable)

import Test.Hspec (Spec, describe, it, shouldBe)

import Text.XML.Light (parseXML, showContent)

import Util.XML (getAllPrefixes, renamePrefixes, toNameBase)

getAllPrefixesExamples :: [(BS.ByteString, [String])]
getAllPrefixesExamples =
  [ ( ""
    , []
    )
  , ("abcd"
    , []
    )
  , ( "<rdf:RDF xmlns:rdf='xxx#' xmlns:iX='http://ns.adobe.com/iX/1.0/'>\
      \<rdf:Description about=''></rdf:Description>"
    , ["iX", "rdf"]
    )
  ]

renamePrefixesExamples :: [(BS.ByteString, BS.ByteString)]
renamePrefixesExamples =
  [ ( ""
    , ""
    )
  , ( "<rdf:RDF xmlns:rdf=\"xxx\" xmlns:iX=\"yyy\">\
      \<rdf:Description about=\"\"></rdf:Description></rdf:RDF>"
    , "<b:RDF xmlns:b=\"xxx\" xmlns:a=\"yyy\">\
      \<b:Description about=\"\" /></b:RDF>"
    )
  , ( "<xxx:tag xmlns:xxx=\"xxx\" xmlns:yyy=\"yyy\">\
      \<xxx:tag2><yyy:tag3>Hello</yyy:tag3>abcd<yyy:tag4>World!Hello</yyy:tag4>\
      \</xxx:tag2></xxx:tag>"
    , "<a:tag xmlns:a=\"xxx\" xmlns:b=\"yyy\">\
      \<a:tag2><b:tag3>Hello</b:tag3>abcd<b:tag4>World!Hello</b:tag4>\
      \</a:tag2></a:tag>"
    )
  ]

spec :: Spec
spec = do
  describe "getAllPrefixes" $ do
    forM_ getAllPrefixesExamples $ \(example, expected) ->
      it ("should work with " ++ show example) $ do
        let prefixes = sort . nub . getAllPrefixes . parseXML $ example

        prefixes `shouldBe` expected

  describe "renamePrefixes" $ do
    forM_ renamePrefixesExamples $ \(example, expected) ->
      it ("should work with " ++ show example) $ do
        let xml = parseXML example
            prefixes    = getAllPrefixes xml
            newPrefixes = getTranslationTable toNameBase prefixes
            renamed     = renamePrefixes newPrefixes xml
            result      = BS.concat (BSU.fromString . showContent <$> renamed)

        result `shouldBe` expected
