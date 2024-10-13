module Pdf.Graphics.Parser.Stream
  ( gfxParse
  ) where

import Control.Applicative ((<|>))

import Data.Array (mkArray)
import Data.Binary.Parser (Get, label, many', parseDetail, peek, satisfy, sepBy)
import Data.ByteString qualified as BS
import Data.Fallible (Fallible)
import Data.PDF.GFXObject (GFXObject, isWhiteSpace)
import Data.PDF.GFXObjects (GFXObjects)
import Data.UnifiedError (UnifiedError (ParseError))
import Data.Word (Word8)

import Pdf.Graphics.Parser.Comment (commentP)
import Pdf.Graphics.Parser.Container (arrayP, dictionaryP)
import Pdf.Graphics.Parser.EmptyContent (emptyContentP)
import Pdf.Graphics.Parser.HexString (hexStringP)
import Pdf.Graphics.Parser.InlineImage (inlineImageP)
import Pdf.Graphics.Parser.Keyword (keywordP)
import Pdf.Graphics.Parser.Name (nameP)
import Pdf.Graphics.Parser.Number (numberP)
import Pdf.Graphics.Parser.String (stringP)

import Util.Ascii
    ( asciiLEFTPARENTHESIS
    , asciiLEFTSQUAREBRACKET
    , asciiLESSTHANSIGN
    , asciiPERCENTSIGN
    , asciiSOLIDUS
    , asciiUPPERB
    )

whiteSpaces :: Get [Word8]
whiteSpaces = many' (satisfy isWhiteSpace)

gfxObjectP :: Get GFXObject
gfxObjectP = do
  nextChar <- peek
  if | nextChar == asciiSOLIDUS           -> nameP
     | nextChar == asciiLESSTHANSIGN      -> dictionaryP <|> hexStringP
     | nextChar == asciiLEFTPARENTHESIS   -> stringP
     | nextChar == asciiLEFTSQUAREBRACKET -> arrayP
     | nextChar == asciiPERCENTSIGN       -> commentP
     | nextChar == asciiUPPERB            -> inlineImageP <|> keywordP
     | otherwise                          -> numberP <|> keywordP

gfxRawP :: Get [GFXObject]
gfxRawP = label "gfxG" $ do
  emptyContentP
  objects <- gfxObjectP `sepBy` whiteSpaces
  emptyContentP
  return objects

{-|
Parses a graphics stream from a bytestring.

It encapsulates `parseDetail` in order to use the `UnifiedError` type when
returning errors.
-}
gfxParse
  :: BS.ByteString -- ^ The bytestring to parse coming from a file.
  -> Fallible GFXObjects -- ^ Error or a `List of `GFXObject`.
gfxParse source = case parseDetail gfxRawP source of
  Left  err                      -> Left (ParseError err)
  Right (""    , _     , result) -> Right (mkArray result)
  Right (remain, offset, _     ) -> Left
    (ParseError (remain, offset, "Stopped to read at offset " ++ show offset))
