module Pdf.Graphics.Parser.Stream
  ( gfxParse
  ) where

import           Control.Applicative            ( (<|>) )
import           Data.Binary.Parser             ( Get
                                                , label
                                                , many'
                                                , parseDetail
                                                , satisfy
                                                , sepBy
                                                )
import qualified Data.ByteString               as BS
import           Data.Word                      ( Word8 )
import           Pdf.Graphics.Object            ( GFXObject
                                                , isWhiteSpace
                                                )
import           Pdf.Graphics.Parser.Comment    ( commentP )
import           Pdf.Graphics.Parser.Container  ( dictionaryP
                                                , arrayP
                                                )
import           Util.UnifiedError                    ( UnifiedError(ParseError) )
import           Pdf.Graphics.Parser.HexString  ( hexStringP )
import           Pdf.Graphics.Parser.Keyword    ( keywordP )
import           Pdf.Graphics.Parser.Name       ( nameP )
import           Pdf.Graphics.Parser.Number     ( numberP )
import           Pdf.Graphics.Parser.String     ( stringP )
import           Pdf.Graphics.Parser.InlineImage
                                                ( inlineImageP )
import           Pdf.Graphics.Parser.EmptyContent
                                                ( emptyContentP )
import           Util.Array                     ( Array
                                                , mkArray
                                                )

whiteSpaces :: Get [Word8]
whiteSpaces = many' (satisfy isWhiteSpace)

gfxObjectP :: Get GFXObject
gfxObjectP =
  commentP
    <|> inlineImageP
    <|> dictionaryP
    <|> arrayP
    <|> stringP
    <|> hexStringP
    <|> nameP
    <|> numberP
    <|> keywordP

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
  -> Either UnifiedError (Array GFXObject) -- ^ Error or a `List of `GFXObject`.
gfxParse source = case parseDetail gfxRawP source of
  Left  err                      -> Left (ParseError err)
  Right (""    , _     , result) -> Right (mkArray result)
  Right (remain, offset, _     ) -> Left
    (ParseError (remain, offset, "Stopped to read at offset " ++ show offset))
