{-|
This module groups all the errors that DietPDF may generate.

Having one type for all errors means the Either monad can be used to avoid
long if then else if then else.
-}
module Data.UnifiedError
  ( UnifiedError(ParseError, UnableToOpenFile, EncodeNoIndirectObject, EncodeNoVersion, EncodeNoTrailer, EncodeNoRootEntry, RLEDecodeError, RLEEncodeError, FlateDecodeError, LZWStopCodeNotFound, NotEnoughBytes, InternalError, InvalidPredictor, InvalidNumberOfBytes, InvalidFilterParm, InvalidAscii85Stream, NoStream, NoDictionary, InvalidObjectToEmbed, NoObjectToEncode, UnknownScalerType, ObjectStreamNotFound, ObjectNotFound, XRefStreamNoW, ExternalCommandError, PDFTKError, UnsupportedFeature, EncodeEncrypted)
  )
where

import Data.Binary.Get (ByteOffset)
import Data.ByteString qualified as BS
import Data.ErrorType
    ( ErrorType (EncodingError, ParsingError, ReadingError, StructureError, UnsupportedError)
    )
import Data.Kind (Type)
import Data.Word (Word8)

type UnifiedError :: Type
data UnifiedError
  -- | Parsing error (Remaining bytes, offset of the error and error message)
  = ParseError !(BS.ByteString, ByteOffset, String)
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
  -- | The PDF file is encrypted
  | EncodeEncrypted
  -- | The decoding of an RLE bytestring generated an error
  | RLEDecodeError !String
  -- | The encoding to an RLE bytestring generated an error
  | RLEEncodeError !String
  -- | The decoding of Zlib bytestring generated an error
  | FlateDecodeError !String
  -- | The decoding of LZW bytestring generated an error
  | LZWStopCodeNotFound
  -- | Not enough bytes in the stream to decode (expected, actual)
  | NotEnoughBytes !Int !Int
  -- | Error that should not be generated
  | InternalError
  -- | Invalid predictor
  | InvalidPredictor !Word8
  -- | Number of bytes is invalid according to the predictor (expected, actual)
  | InvalidNumberOfBytes !Int !Int
  -- | Invalid combination of Filter and DecodeParms
  | InvalidFilterParm !String
  -- | Invalid Ascii85 stream
  | InvalidAscii85Stream !String
  -- | No stream in the object
  | NoStream !String
  -- | No dictionary in the object
  | NoDictionary !String
  -- | Invalid object to embed
  | InvalidObjectToEmbed !String
  -- | No object to encode
  | NoObjectToEncode
  | UnknownScalerType !String
  | ObjectStreamNotFound
  | ObjectNotFound
  | XRefStreamNoW !String
  | ExternalCommandError !String !Int
  | PDFTKError !String
  | UnsupportedFeature !String
  deriving stock (Eq)

errorType :: UnifiedError -> ErrorType
errorType (ParseError _)             = ParsingError
errorType UnableToOpenFile           = ReadingError
errorType ObjectNotFound             = ReadingError
errorType (ExternalCommandError _ _) = ReadingError
errorType (PDFTKError _)             = ReadingError
errorType EncodeNoIndirectObject     = EncodingError
errorType EncodeNoVersion            = EncodingError
errorType EncodeEncrypted            = EncodingError
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
errorType (InvalidFilterParm    _)   = EncodingError
errorType (NoStream             _)   = StructureError
errorType (NoDictionary         _)   = StructureError
errorType (InvalidObjectToEmbed _)   = StructureError
errorType (XRefStreamNoW _)          = StructureError
errorType NoObjectToEncode           = EncodingError
errorType (UnknownScalerType _)      = ParsingError
errorType ObjectStreamNotFound       = ParsingError
errorType (UnsupportedFeature _)     = UnsupportedError

show' :: UnifiedError -> String -> String
show' err msg = concat ["[", show (errorType err), "] ", msg]

instance Show UnifiedError where
  show :: UnifiedError -> String
  show err@(ParseError (_, _, msg))         = show' err msg
  show err@UnableToOpenFile                 = show' err "Unable to open file"
  show err@ObjectNotFound                   = show' err "Object not found"
  show err@EncodeNoIndirectObject = show' err "No indirect object to encode"
  show err@EncodeNoVersion                  = show' err "No version to encode"
  show err@EncodeEncrypted                  = show' err "PDF is encrypted"
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
  show err@(InvalidFilterParm msg) =
    show' err ("Invalid combination of Filter and DecodeParms: " ++ msg)
  show err@(InvalidAscii85Stream msg) =
    show' err ("Invalid Ascii85 stream: " ++ msg)
  show err@(NoStream     msg) = show' err ("No stream: " ++ msg)
  show err@(NoDictionary msg) = show' err ("No dictionary: " ++ msg)
  show err@(InvalidObjectToEmbed msg) =
    show' err ("Invalid object to embed: " ++ msg)
  show err@NoObjectToEncode        = show' err "No object to encode"
  show err@(UnknownScalerType msg) = show' err ("Unknown scaler type: " ++ msg)
  show err@ObjectStreamNotFound    = show' err "Object stream not found"
  show err@(XRefStreamNoW msg) =
    show' err ("XRef stream with invalid or no W field: " ++ msg)
  show err@(ExternalCommandError msg rc) =
    show' err ("External command error for " ++ msg ++ " (" ++ show rc ++ ")")
  show err@(PDFTKError msg) = show' err ("PDFTK error: " ++ msg)
  show err@(UnsupportedFeature msg) =
    show' err ("Unsupported feature: " ++ msg)
