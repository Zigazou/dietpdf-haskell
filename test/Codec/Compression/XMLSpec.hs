module Codec.Compression.XMLSpec
  ( spec
  ) where

import Codec.Compression.XML (optimizeXML)

import Control.Monad (forM_)

import Data.ByteString qualified as BS

import Test.Hspec (Spec, describe, it, shouldBe)

xmlExamples :: [(BS.ByteString, BS.ByteString)]
xmlExamples =
  [ ( "<?xml version=\"1.0\" ?>\n\
      \<a>aa</a>\n"
    , "<?xml version=\"1.0\" ?><a>aa</a>"
    )
  , ( "<?xml version=\"1.0\" ?>\n\
      \    <a>  </a>\n"
    , "<?xml version=\"1.0\" ?><a>  </a>"
    )
  , ( "<?xml version=\"1.0\" ?>\n\
      \<a>\n\
      \  <b>\n\
      \    <c>cc</c>\n\
      \  </b>\n\
      \</a>\n"
    , "<?xml version=\"1.0\" ?><a><b><c>cc</c></b></a>"
    )
  , ( "<?xml version=\"1.0\" ?>\n\
      \             \n\
      \       \t    \n\
      \    <a>  </a>\n"
    , "<?xml version=\"1.0\" ?><a>  </a>"
    )
  , ( "<?xpacket begin=\"\xEF\xBB\xBF\" id=\"W5M0MpCehiHzreSzNTczkc9d\" ?>\
      \<x:xmpmeta xmlns:x=\"adobe:ns:meta/\">Fr\xC3\xA9\x64\xC3\xA9ric</x:xmpmeta>\n    \n    \n\
      \<?xpacket end=\"w\"?>"
    , "<?xpacket begin=\"\xEF\xBB\xBF\" id=\"W5M0MpCehiHzreSzNTczkc9d\" ?>\
      \<x:xmpmeta xmlns:x=\"adobe:ns:meta/\">Fr\xC3\xA9\x64\xC3\xA9ric</x:xmpmeta>\
      \<?xpacket end=\"w\" ?>"
    )
  , ( "<?xpacket begin='' id='W5M0MpCehiHzreSzNTczkc9d' bytes='1691'?>\n\
    \\n\
    \<rdf:RDF xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#'\n\
    \ xmlns:iX='http://ns.adobe.com/iX/1.0/'>\n\
    \\n\
    \ <rdf:Description about=''\n\
    \  xmlns='http://ns.adobe.com/pdf/1.3/'\n\
    \  xmlns:pdf='http://ns.adobe.com/pdf/1.3/'>\n\
    \  <pdf:CreationDate>2001-11-17T17:10:08Z</pdf:CreationDate>\n\
    \  <pdf:ModDate>2001-11-29T09:16:56-08:00</pdf:ModDate>\n\
    \  <pdf:Producer>Acrobat Distiller 5.00 for Macintosh</pdf:Producer>\n\
    \  <pdf:Author>Adobe Systems Incorporated</pdf:Author>\n\
    \  <pdf:Creator>FrameMaker 6.0</pdf:Creator>\n\
    \  <pdf:Title>PDF Reference, Third Edition</pdf:Title>\n\
    \  <pdf:Subject>Adobe Portable Document Format (PDF)</pdf:Subject>\n\
    \ </rdf:Description>\n\
    \\n\
    \ <rdf:Description about=''\n\
    \  xmlns='http://ns.adobe.com/xap/1.0/'\n\
    \  xmlns:xap='http://ns.adobe.com/xap/1.0/'>\n\
    \  <xap:CreateDate>2001-11-17T17:10:08Z</xap:CreateDate>\n\
    \  <xap:ModifyDate>2001-11-29T09:16:56-08:00</xap:ModifyDate>\n\
    \  <xap:Author>Adobe Systems Incorporated</xap:Author>\n\
    \  <xap:MetadataDate>2001-11-29T09:16:56-08:00</xap:MetadataDate>\n\
    \  <xap:Title>\n\
    \   <rdf:Alt>\n\
    \    <rdf:li xml:lang='x-default'>PDF Reference, Third Edition</rdf:li>\n\
    \   </rdf:Alt>\n\
    \  </xap:Title>\n\
    \  <xap:Description>\n\
    \   <rdf:Alt>\n\
    \    <rdf:li xml:lang='x-default'>Adobe Portable Document Format (PDF)</rdf:li>\n\
    \   </rdf:Alt>\n\
    \  </xap:Description>\n\
    \ </rdf:Description>\n\
    \\n\
    \ <rdf:Description about=''\n\
    \  xmlns='http://purl.org/dc/elements/1.1/'\n\
    \  xmlns:dc='http://purl.org/dc/elements/1.1/'>\n\
    \  <dc:creator>Adobe Systems Incorporated</dc:creator>\n\
    \  <dc:title>PDF Reference, Third Edition</dc:title>\n\
    \  <dc:description>Adobe Portable Document Format (PDF)</dc:description>\n\
    \ </rdf:Description>\n\
    \\n\
    \</rdf:RDF>\n\
    \<?xpacket end='r'?>"
    , "<?xpacket begin=\"\" id=\"W5M0MpCehiHzreSzNTczkc9d\" bytes=\"1691\" ?><rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:iX=\"http://ns.adobe.com/iX/1.0/\"><rdf:Description about=\"\" xmlns=\"http://ns.adobe.com/pdf/1.3/\" xmlns:pdf=\"http://ns.adobe.com/pdf/1.3/\"><pdf:CreationDate>2001-11-17T17:10:08Z</pdf:CreationDate><pdf:ModDate>2001-11-29T09:16:56-08:00</pdf:ModDate><pdf:Producer>Acrobat Distiller 5.00 for Macintosh</pdf:Producer><pdf:Author>Adobe Systems Incorporated</pdf:Author><pdf:Creator>FrameMaker 6.0</pdf:Creator><pdf:Title>PDF Reference, Third Edition</pdf:Title><pdf:Subject>Adobe Portable Document Format (PDF)</pdf:Subject></rdf:Description><rdf:Description about=\"\" xmlns=\"http://ns.adobe.com/xap/1.0/\" xmlns:xap=\"http://ns.adobe.com/xap/1.0/\"><xap:CreateDate>2001-11-17T17:10:08Z</xap:CreateDate><xap:ModifyDate>2001-11-29T09:16:56-08:00</xap:ModifyDate><xap:Author>Adobe Systems Incorporated</xap:Author><xap:MetadataDate>2001-11-29T09:16:56-08:00</xap:MetadataDate><xap:Title><rdf:Alt><rdf:li xml:lang=\"x-default\">PDF Reference, Third Edition</rdf:li></rdf:Alt></xap:Title><xap:Description><rdf:Alt><rdf:li xml:lang=\"x-default\">Adobe Portable Document Format (PDF)</rdf:li></rdf:Alt></xap:Description></rdf:Description><rdf:Description about=\"\" xmlns=\"http://purl.org/dc/elements/1.1/\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\"><dc:creator>Adobe Systems Incorporated</dc:creator><dc:title>PDF Reference, Third Edition</dc:title><dc:description>Adobe Portable Document Format (PDF)</dc:description></rdf:Description></rdf:RDF><?xpacket end=\"r\" ?>"
    )
  ]

spec :: Spec
spec = describe "optimizeXML" $ do
  forM_ xmlExamples $ \(example, expected) ->
    it ("should work with " ++ show example)
      $          optimizeXML example
      `shouldBe` expected
