{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}
{-|
This module groups all the errors that DietPDF may generate.

Having one type for all errors means the Either monad can be used to avoid
long if then else if then else.
-}
module Util.Errors
  ( UnifiedError(..)
  ) where
import qualified Data.ByteString               as BS
import           Data.Word                      ( Word8 )
import           Data.Binary.Get                ( ByteOffset )

data ErrorType = ParsingError | EncodingError deriving stock Eq

instance Show ErrorType where
  show ParsingError  = "parsing"
  show EncodingError = "encoding"

data UnifiedError
  -- | Parsing error (Remaining bytes, offset of the error and error message)
  = ParseError (BS.ByteString, ByteOffset, String)
  -- | The PDF file contains no indirect object
  | EncodeNoIndirectObject
  -- | The PDF file contains no version comment
  | EncodeNoVersion
  -- | The PDF file contains no trailer object
  | EncodeNoTrailer
  -- | The PDF file contains no root entry
  | EncodeNoRootEntry
  -- | The decoding of an RLE bytestring generated an error
  | RLEDecodeError String
  -- | The encoding to an RLE bytestring generated an error
  | RLEEncodeError String
  -- | The decoding of Zlib bytestring generated an error
  | FlateDecodeError String
  -- | The decoding of LZW bytestring generated an error
  | LZWStopCodeNotFound
  -- | Not enough bytes in the stream to decode (expected, actual)
  | NotEnoughBytes Int Int
  -- | Error that should not be generated
  | InternalError
  -- | Invalid predictor
  | InvalidPredictor Word8
  -- | Number of bytes is invalid according to the predictor (expected, actual)
  | InvalidNumberOfBytes Int Int
  -- | Invalid combination of Filter and DecodeParms
  | InvalidFilterParm
  deriving stock Eq

errorType :: UnifiedError -> ErrorType
errorType (ParseError _)             = ParsingError
errorType EncodeNoIndirectObject     = EncodingError
errorType EncodeNoVersion            = EncodingError
errorType EncodeNoTrailer            = EncodingError
errorType EncodeNoRootEntry          = EncodingError
errorType (RLEEncodeError   _)       = EncodingError
errorType (RLEDecodeError   _)       = ParsingError
errorType (FlateDecodeError _)       = ParsingError
errorType LZWStopCodeNotFound        = ParsingError
errorType (NotEnoughBytes _ _)       = ParsingError
errorType InternalError              = ParsingError
errorType (InvalidPredictor _      ) = ParsingError
errorType (InvalidNumberOfBytes _ _) = ParsingError
errorType InvalidFilterParm          = EncodingError

show' :: UnifiedError -> String -> String
show' err msg = concat ["[", show (errorType err), "] ", msg]

instance Show UnifiedError where
  show err@(ParseError (_, _, msg))         = show' err msg
  show err@EncodeNoIndirectObject = show' err "No indirect object to encode"
  show err@EncodeNoVersion                  = show' err "No version to encode"
  show err@EncodeNoTrailer                  = show' err "No trailer to encode"
  show err@EncodeNoRootEntry = show' err "No root entry to encode"
  show err@(RLEEncodeError   msg)           = show' err msg
  show err@(RLEDecodeError   msg)           = show' err msg
  show err@(FlateDecodeError msg)           = show' err msg
  show err@LZWStopCodeNotFound = show' err "LZW Stop code not found"
  show err@(NotEnoughBytes expected actual) = show'
    err
    (concat
      [ "Expected at least "
      , show expected
      , " bytes, received only "
      , show actual
      , " bytes"
      ]
    )
  show err@InternalError = show' err "Internal error"
  show err@(InvalidPredictor actual) =
    show' err (concat ["Invalid predictor (", show actual, ")"])
  show err@(InvalidNumberOfBytes expected actual) = show'
    err
    (concat
      [ "Number of bytes must be a multiple of "
      , show expected
      , " bytes, actual length is "
      , show actual
      , " bytes"
      ]
    )
  show err@InvalidFilterParm =
    show' err "Invalid combination of Filter and DecodeParms"
