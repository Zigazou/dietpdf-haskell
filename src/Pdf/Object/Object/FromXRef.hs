module Pdf.Object.Object.FromXRef
  ( fromXRef
  ) where

import Data.ByteString qualified as BS
import Data.Text qualified as TS
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy qualified as TL

import Formatting (format, int, left, (%))
import Formatting.ByteStringFormatter (utf8)

import Pdf.Object.Object.XRefEntry
    ( XRefEntry (xreGeneration, xreOffset, xreState)
    )
import Pdf.Object.Object.XRefState (XRefState (InUseEntry))
import Pdf.Object.Object.XRefSubsection
    ( XRefSubsection (xrssCount, xrssEntries, xrssStart)
    )

fromXRefEntry :: XRefEntry -> BS.ByteString
fromXRefEntry xre = encodeUtf8 . TL.toStrict $ format
  (left 10 '0' % " " % left 5 '0' % " " % utf8 % "\r\n")
  (xreOffset xre)
  (xreGeneration xre)
  (if xreState xre == InUseEntry then "n" else "f")

fromXRefSubsection :: XRefSubsection -> BS.ByteString
fromXRefSubsection xrss = BS.concat
  [encodeUtf8 subsectionInfo, BS.concat (fromXRefEntry <$> xrssEntries xrss)]
 where
  subsectionInfo :: TS.Text
  subsectionInfo = TL.toStrict
    $ format ("" % int % " " % int % "\n") (xrssStart xrss) (xrssCount xrss)

fromXRef :: [XRefSubsection] -> BS.ByteString
fromXRef xrss = BS.concat ["xref\n", BS.concat (fmap fromXRefSubsection xrss)]
