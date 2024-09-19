module Jpeg.Jpeg
( Jpeg
, jpegP
, encode
, jpgParse
) where

import Control.Applicative (many)

import Data.Binary (Get)
import Data.Binary.Get (label)
import Data.Binary.Parser (parseDetail)
import Data.ByteString qualified as BS
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.Kind (Type)

import Jpeg.Segment (Segment, segmentP)
import Jpeg.Segment qualified as Segment (encode)

import Util.Array (Array, mkArray)
import Util.UnifiedError (UnifiedError (ParseError))

type Jpeg :: Type
type Jpeg = Array Segment

encode :: Jpeg -> BS.ByteString
encode jpeg = BS.concat (Segment.encode <$> toList jpeg)

jpegP :: Get Jpeg
jpegP = label "jpeg" $ many segmentP <&> mkArray

{-|
Parses a graphics stream from a bytestring.

It encapsulates `parseDetail` in order to use the `UnifiedError` type when
returning errors.
-}
jpgParse
  :: BS.ByteString -- ^ The bytestring to parse coming from a file.
  -> Either UnifiedError Jpeg -- ^ Error or a `Jpeg`.
jpgParse source = case parseDetail jpegP source of
  Left  err                      -> Left (ParseError err)
  Right (""    , _     , result) -> Right result
  Right (remain, offset, _     ) -> Left
    (ParseError (remain, offset, "Stopped to read at offset " ++ show offset))
