{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

{-|
This module groups all the errors that DietPDF may generate.

Having one type for all errors means the Either monad can be used to avoid
long if then else if then else.
-}
module Util.Errors
  ( UnifiedError(..)
  , putError
  , putErrorLn
  , unifiedError
  ) where

import           Control.Monad.State            ( lift )
import           Control.Monad.Trans.Class      ( MonadTrans )
import           Data.Binary.Get                ( ByteOffset )
import qualified Data.ByteString               as BS
import           Data.Word                      ( Word8 )
import           System.IO                      ( stderr )
import           Util.Ascii                     ( asciiLF )

data ErrorType = ReadingError
               | ParsingError
               | EncodingError
               | StructureError
               deriving stock Eq

instance Show ErrorType where
  show :: ErrorType -> String
  show ReadingError   = "reading"
  show ParsingError   = "parsing"
  show EncodingError  = "encoding"
  show StructureError = "structure"

data UnifiedError
  -- | Parsing error (Remaining bytes, offset of the error and error message)
  = ParseError (BS.ByteString, ByteOffset, String)
  -- | The PDF file cannot be opened
  | UnableToOpenFile
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
  -- | Invalid Ascii85 stream
  | InvalidAscii85Stream String
  -- | No stream in the object
  | NoStream String
  -- | No dictionary in the object
  | NoDictionary String
  -- | Invalid object to embed
  | InvalidObjectToEmbed String
  -- | No object to encode
  | NoObjectToEncode
  | UnknownScalerType String
  deriving stock (Eq)

{- |
In a monad transformer having `Either` `UnifiedError` as type, the
`unifiedError` allows to directly generate the error without lifting the
error.
-}
unifiedError :: MonadTrans t => UnifiedError -> t (Either UnifiedError) a
unifiedError = lift . Left

errorType :: UnifiedError -> ErrorType
errorType (ParseError _)             = ParsingError
errorType UnableToOpenFile           = ReadingError
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
errorType (InvalidAscii85Stream _  ) = ParsingError
errorType (InvalidPredictor     _  ) = ParsingError
errorType (InvalidNumberOfBytes _ _) = ParsingError
errorType InvalidFilterParm          = EncodingError
errorType (NoStream             _)   = StructureError
errorType (NoDictionary         _)   = StructureError
errorType (InvalidObjectToEmbed _)   = StructureError
errorType NoObjectToEncode           = EncodingError
errorType (UnknownScalerType _) = ParsingError

show' :: UnifiedError -> String -> String
show' err msg = concat ["[", show (errorType err), "] ", msg]

instance Show UnifiedError where
  show :: UnifiedError -> String
  show err@(ParseError (_, _, msg))         = show' err msg
  show err@UnableToOpenFile                 = show' err "Unable to open file"
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
  show err@(InvalidAscii85Stream msg) =
    show' err ("Invalid Ascii85 stream: " ++ msg)
  show err@(NoStream     msg) = show' err ("No stream: " ++ msg)
  show err@(NoDictionary msg) = show' err ("No dictionary: " ++ msg)
  show err@(InvalidObjectToEmbed msg) =
    show' err ("Invalid object to embed: " ++ msg)
  show err@NoObjectToEncode = show' err "No object to encode"
  show err@(UnknownScalerType msg) =
    show' err ("Unknown scaler type: " ++ msg)

{- |
Output a `ByteString` on the standard error output.

This function outputs the bytes as is without appending a line return.
-}
putError :: BS.ByteString -> IO ()
putError = BS.hPutStr stderr

{- |
Output a `ByteString` on the standard error output.

This function appends a line return (`asciiLF`) to the output.
-}
putErrorLn :: BS.ByteString -> IO ()
putErrorLn msg =
  BS.hPutStr stderr msg >> BS.hPutStr stderr (BS.singleton asciiLF)
