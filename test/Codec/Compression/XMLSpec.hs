module Codec.Compression.XMLSpec
  ( spec
  ) where

import Codec.Compression.XML (optimizeXML)

import Control.Monad (forM_)

import Data.ByteString qualified as BS
import Data.PDF.PDFWork (evalPDFWorkT)

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
      \<a:xmpmeta xmlns:a=\"adobe:ns:meta/\">Fr\xC3\xA9\x64\xC3\xA9ric</a:xmpmeta>\
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
    , "<?xpacket begin=\"\" id=\"W5M0MpCehiHzreSzNTczkc9d\" bytes=\"1691\" ?><d:RDF xmlns:d=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:b=\"http://ns.adobe.com/iX/1.0/\"><d:Description about=\"\" xmlns=\"http://ns.adobe.com/pdf/1.3/\" xmlns:c=\"http://ns.adobe.com/pdf/1.3/\"><c:CreationDate>2001-11-17T17:10:08Z</c:CreationDate><c:ModDate>2001-11-29T09:16:56-08:00</c:ModDate><c:Producer>Acrobat Distiller 5.00 for Macintosh</c:Producer><c:Author>Adobe Systems Incorporated</c:Author><c:Creator>FrameMaker 6.0</c:Creator><c:Title>PDF Reference, Third Edition</c:Title><c:Subject>Adobe Portable Document Format (PDF)</c:Subject></d:Description><d:Description about=\"\" xmlns=\"http://ns.adobe.com/xap/1.0/\" xmlns:e=\"http://ns.adobe.com/xap/1.0/\"><e:CreateDate>2001-11-17T17:10:08Z</e:CreateDate><e:ModifyDate>2001-11-29T09:16:56-08:00</e:ModifyDate><e:Author>Adobe Systems Incorporated</e:Author><e:MetadataDate>2001-11-29T09:16:56-08:00</e:MetadataDate><e:Title><d:Alt><d:li f:lang=\"x-default\">PDF Reference, Third Edition</d:li></d:Alt></e:Title><e:Description><d:Alt><d:li f:lang=\"x-default\">Adobe Portable Document Format (PDF)</d:li></d:Alt></e:Description></d:Description><d:Description about=\"\" xmlns=\"http://purl.org/dc/elements/1.1/\" xmlns:a=\"http://purl.org/dc/elements/1.1/\"><a:creator>Adobe Systems Incorporated</a:creator><a:title>PDF Reference, Third Edition</a:title><a:description>Adobe Portable Document Format (PDF)</a:description></d:Description></d:RDF><?xpacket end=\"r\" ?>"
    )
  ]

spec :: Spec
spec = describe "optimizeXML" $ do
  forM_ xmlExamples $ \(example, expected) ->
    it ("should work with " ++ show example) $ do
      optimized <- evalPDFWorkT (optimizeXML example)
      optimized `shouldBe` Right expected
