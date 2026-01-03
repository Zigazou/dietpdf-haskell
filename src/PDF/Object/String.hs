{-|
PDF string optimization utilities

This module provides utilities for optimizing PDF string objects by converting
they hex string format into more compact encodings.

PDF strings can be encoded in multiple ways. Hex strings (PDFHexString) use
twice the space of regular strings (PDFString) because each byte is represented
as two hexadecimal digits. This module detects when hex strings can be safely
converted to more compact encodings without data loss.
-}
module PDF.Object.String
  ( optimizeString
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Context (Contextual (ctx))
import Data.Logging (Logging)
import Data.Map qualified as Map
import Data.PDF.PDFWork (PDFWork, sayComparisonP, withContext)

import PDF.Object.Object (PDFObject (PDFHexString, PDFString))

import Util.Ascii (asciiDELETE)
import Util.PdfDocEncoding (unicodeToPdfDocEncoding)
import Util.String (hexStringToString)

{-|
UTF-16 Big-Endian byte order marker (BOM).

The two-byte sequence that identifies UTF-16 BE encoded text: 0xFE 0xFF. Used to
detect whether a string is UTF-16 BE encoded.
-}
utf16beBOM :: ByteString
utf16beBOM = "\xfe\xff"

{-|
Convert a UTF-16 BE encoded string to PDF Doc Encoding if possible.

Attempts to decode a UTF-16 BE encoded bytestring and re-encode it using PDF Doc
Encoding. Only succeeds if all characters in the UTF-16 BE string have
corresponding PDF Doc Encoding representations.

PDF Doc Encoding is a superset of ISO 8859-1 (Latin-1) with additional
characters in the 0x80-0x9F range. This conversion allows UTF-16 BE strings
containing only PDF Doc Encoding characters to be stored in the more compact PDF
string format (4x compression compared to hex strings).

__Parameters:__

- A UTF-16 BE encoded bytestring (must start with BOM, must have even length)

__Returns:__ 'Just' the PDF Doc Encoding if all characters can be represented,
or 'Nothing' if the string cannot be converted (e.g., contains characters
outside PDF Doc Encoding).
-}
utf16beToPdfDocEncoding :: ByteString -> Maybe ByteString
utf16beToPdfDocEncoding utf16beString
  | BS.length utf16beString < 2           = Nothing
  | odd (BS.length utf16beString)         = Nothing
  | BS.take 2 utf16beString /= utf16beBOM = Nothing
  | otherwise                             = convert (BS.drop 2 utf16beString)
 where
  convert :: ByteString -> Maybe ByteString
  convert bytes =
    case BS.length bytes of
      0               -> Just ""
      1               -> Nothing
      _anyOtherLength -> do
        byte <- Map.lookup (codepoint (BS.take 2 bytes)) unicodeToPdfDocEncoding
        remains <- convert (BS.drop 2 bytes)
        return $ BS.cons byte remains

  codepoint :: ByteString -> Int
  codepoint twoBytes = case BS.unpack (BS.take 2 twoBytes) of
    [first, second] -> fromIntegral first * 256 + fromIntegral second
    _anythingElse   -> 0

{-|
Check if a bytestring is UTF-16 Big-Endian encoded.

Determines whether a bytestring starts with the UTF-16 BE byte order marker
(0xFE 0xFF), indicating UTF-16 BE encoding.

__Parameters:__

- A bytestring to check

__Returns:__ 'True' if the string starts with the UTF-16 BE BOM and is at least
2 bytes long, 'False' otherwise.
-}
isUTF16Encoded :: ByteString -> Bool
isUTF16Encoded string | BS.length string < 2           = False
                      | BS.take 2 string /= utf16beBOM = False
                      | otherwise                      = True

{-|
Check if a bytestring contains only ASCII characters.

Determines whether all bytes in a bytestring are in the ASCII range (0-127).
ASCII strings can be optimized by converting from hex string format to regular
string format with 50% size reduction.

__Parameters:__

- A bytestring to check

__Returns:__ 'True' if all bytes are <= 127 (ASCII range), 'False' if any byte
is in the extended range (128-255).
-}
isASCIIEncoded :: ByteString -> Bool
isASCIIEncoded = BS.all (<= asciiDELETE)

{-|
Optimize PDF string objects by converting hex strings to more compact formats.

Attempts to convert 'PDFHexString' objects to 'PDFString' format using the most
compact encoding possible. Hex strings use twice the space of regular strings,
so conversion yields significant size reduction when applicable.

__Optimization strategies:__

1. __ASCII encoding__ (50% compression): If the hex string contains only ASCII
   characters (0-127), convert directly to PDFString. Example: 48656C6C6F →
   Hello (6 bytes to 5 bytes)

2. __PDFDocEncoding__ (75% compression for UTF-16): If the hex string is UTF-16
   BE encoded but contains only PDFDocEncoding characters, decode and re-encode
   using PDFDocEncoding instead. This reduces 12 bytes of UTF-16 hex to 3 bytes
   of PDFDocEncoding.

3. __UTF-16BE preservation__: If UTF-16 BE encoded but not convertible to
   PDFDocEncoding, still converts from hex string to binary string for 50%
   compression.

4. __No optimization__: If encoding cannot be determined or is already optimal,
   returns the object unchanged.

Logging reports the optimization type (ASCII, PDFDocEncoding, or UTF-16BE) and
the size reduction achieved.

__Parameters:__

- A PDF object (may be PDFHexString, PDFString, or other object types)

__Returns:__ The optimized PDF object in the 'PDFWork' monad.

__Side effects:__ Logs comparison information using 'sayComparisonP' for each
optimization performed.
-}
optimizeString :: Logging m => PDFObject -> PDFWork m PDFObject
optimizeString object =
  withContext (ctx ("optimizeString" :: String) <> ctx object) $
    case object of
    (PDFHexString values)
      | isASCIIEncoded encoded -> do
          -- If the hex string contains only ASCII characters, converts it to a
          -- PDFString which will be twice shorter.
          sayComparisonP
            "Hex string optimization (ASCII)"
            (BS.length values)
            (BS.length encoded)
          return $ PDFString encoded
      | isUTF16Encoded encoded -> case utf16beToPdfDocEncoding encoded of
          -- If the PDFHexString is UTF-16 but only contains PDFDocEncoding
          -- characters, converts it to a PDFString which will be four times
          -- shorter.
          Just pdfDocEncoded -> do
            sayComparisonP
              "Hex string optimization (PDFDocEncoding)"
              (BS.length values)
              (BS.length pdfDocEncoded)
            return $ PDFString pdfDocEncoded
          Nothing -> do
            let utf16beEncoded = hexStringToString values
            sayComparisonP
              "Hex string optimization (UTF-16BE)"
              (BS.length values)
              (BS.length utf16beEncoded)
            return $ PDFString utf16beEncoded
      | otherwise -> return object
      where encoded = hexStringToString values
    (PDFString encoded)
      | isUTF16Encoded encoded -> case utf16beToPdfDocEncoding encoded of
          -- If the PDFHexString is UTF-16 but only contains ASCII characters,
          -- converts it to a PDFString which will be four times shorter.
          Just pdfDocEncoded -> do
            sayComparisonP
              "Hex string optimization (PDFDocEncoding)"
              (BS.length encoded)
              (BS.length pdfDocEncoded)
            return $ PDFString pdfDocEncoded
          Nothing -> return object
      | otherwise -> return object
    _anyOtherObject -> return object
