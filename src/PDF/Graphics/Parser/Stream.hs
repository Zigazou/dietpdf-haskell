{-|
Parser for complete PDF graphics streams

This module provides the main entry point for parsing PDF graphics streams. It
coordinates the parsing of graphics objects from a raw bytestring and converts
them into a structured graphics object array.
-}
module PDF.Graphics.Parser.Stream
  ( gfxParse
  ) where

import Control.Applicative ((<|>))

import Data.Array (mkArray)
import Data.Binary.Parser (Get, label, many', parseDetail, peek, satisfy, sepBy)
import Data.ByteString (ByteString)
import Data.Fallible (Fallible)
import Data.PDF.GFXObject (GFXObject, isWhiteSpace)
import Data.PDF.GFXObjects (GFXObjects)
import Data.UnifiedError (UnifiedError (ParseError))
import Data.Word (Word8)

import PDF.Graphics.Parser.Comment (commentP)
import PDF.Graphics.Parser.Container (arrayP, dictionaryP)
import PDF.Graphics.Parser.EmptyContent (emptyContentP)
import PDF.Graphics.Parser.HexString (hexStringP)
import PDF.Graphics.Parser.InlineImage (inlineImageP)
import PDF.Graphics.Parser.Keyword (keywordP)
import PDF.Graphics.Parser.Name (nameP)
import PDF.Graphics.Parser.Number (numberP)
import PDF.Graphics.Parser.String (stringP)

import Util.Ascii
    ( pattern AsciiLEFTPARENTHESIS
    , pattern AsciiLEFTSQUAREBRACKET
    , pattern AsciiLESSTHANSIGN
    , pattern AsciiPERCENTSIGN
    , pattern AsciiSOLIDUS
    , pattern AsciiUPPERB
    )

{-|
Parse zero or more whitespace characters.

This parser consumes any number of whitespace bytes (including zero) and returns
them as a list. It is typically used to skip separators between graphics
objects.
-}
whiteSpaces :: Get [Word8]
whiteSpaces = many' (satisfy isWhiteSpace)

{-|
Parse a single graphics object by dispatching to the appropriate parser.

This function peeks at the next byte to determine the type of object being
parsed and delegates to the corresponding parser:

- @/@: name object
- @<@: dictionary or hex string
- @(@: string object
- @[@: array object
- @%@: comment
- @B@: inline image or keyword
- Other: number or keyword

The dispatcher uses greedy parsing, trying more specific parsers first where
ambiguity exists (e.g., @<@ could start either a dictionary or hex string).
-}
gfxObjectP :: Get GFXObject
gfxObjectP = peek >>= \case
  AsciiSOLIDUS           -> nameP
  AsciiLESSTHANSIGN      -> dictionaryP <|> hexStringP
  AsciiLEFTPARENTHESIS   -> stringP
  AsciiLEFTSQUAREBRACKET -> arrayP
  AsciiPERCENTSIGN       -> commentP
  AsciiUPPERB            -> inlineImageP <|> keywordP
  _anyOtherCharacter     -> numberP <|> keywordP

{-|
Parse a sequence of graphics objects from a stream.

This parser handles the complete parsing structure of a graphics stream:

1. Skips leading empty content (whitespace and comments)
2. Parses zero or more graphics objects separated by whitespace
3. Skips trailing empty content

Returns a list of parsed graphics objects in order.
-}
gfxRawP :: Get [GFXObject]
gfxRawP = label "gfxG" $ do
  emptyContentP
  objects <- gfxObjectP `sepBy` whiteSpaces
  emptyContentP
  return objects

{-|
Parse a graphics stream from a bytestring.

This is the main entry point for parsing PDF graphics streams. It takes a raw
bytestring (typically extracted from a PDF content stream) and parses it into a
structured array of graphics objects.

The function uses 'parseDetail' internally to obtain detailed error information
and wraps errors in the 'UnifiedError' type for consistent error handling.

__Parameters:__

- The bytestring to parse, typically from a PDF file

__Returns:__

- 'Right' containing a graphics object array on success
- 'Left' containing a parse error if parsing fails or if not all input is
  consumed
-}
gfxParse
  :: ByteString -- ^ The bytestring to parse coming from a file.
  -> Fallible GFXObjects -- ^ Error or a `List of `GFXObject`.
gfxParse source = case parseDetail gfxRawP source of
  Left  err                      -> Left (ParseError err)
  Right (""    , _     , result) -> Right (mkArray result)
  Right (remain, offset, _     ) -> Left
    (ParseError (remain, offset, "Stopped to read at offset " ++ show offset))
