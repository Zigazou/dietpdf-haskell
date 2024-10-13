module PDF.Object.Object.FromXRef
  ( fromXRef
  ) where

import Data.ByteString qualified as BS
import Data.PDF.XRefEntry (XRefEntry (xreGeneration, xreOffset, xreState))
import Data.PDF.XRefState (XRefState (InUseEntry))
import Data.PDF.XRefSubsection
    ( XRefSubsection (xrssCount, xrssEntries, xrssStart)
    )
import Data.Text qualified as TS
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy qualified as TL

import Formatting (format, int, left, (%))
import Formatting.ByteStringFormatter (utf8)

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
