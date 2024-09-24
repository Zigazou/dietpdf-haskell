module Pdf.Object.String
  ( optimizeString
  ) where

import Data.ByteString qualified as BS
import Data.Map qualified as Map

import Pdf.Object.Object (PDFObject (PDFHexString, PDFString))

import Util.Ascii (asciiDELETE)
import Util.Logging (Logging, sayComparisonF)
import Util.PdfDocEncoding (unicodeToPdfDocEncoding)
import Util.String (hexStringToString)
import Util.UnifiedError (FallibleT)
import Util.Context (Contextual(ctx))

utf16beBOM :: BS.ByteString
utf16beBOM = "\xfe\xff"

utf16beToPdfDocEncoding :: BS.ByteString -> Maybe BS.ByteString
utf16beToPdfDocEncoding utf16beString
  | BS.length utf16beString < 2           = Nothing
  | odd (BS.length utf16beString)         = Nothing
  | BS.take 2 utf16beString /= utf16beBOM = Nothing
  | otherwise                             = convert (BS.drop 2 utf16beString)
 where
  convert :: BS.ByteString -> Maybe BS.ByteString
  convert bytes =
    case BS.length bytes of
      0               -> Just ""
      1               -> Nothing
      _anyOtherLength -> do
        byte <- Map.lookup (codepoint (BS.take 2 bytes)) unicodeToPdfDocEncoding
        remains <- convert (BS.drop 2 bytes)
        return $ BS.cons byte remains

  codepoint :: BS.ByteString -> Int
  codepoint twoBytes = case BS.unpack (BS.take 2 twoBytes) of
    [first, second] -> fromIntegral first * 256 + fromIntegral second
    _anythingElse   -> 0

isUTF16Encoded :: BS.ByteString -> Bool
isUTF16Encoded string | BS.length string < 2           = False
                      | BS.take 2 string /= utf16beBOM = False
                      | otherwise                      = True

isASCIIEncoded :: BS.ByteString -> Bool
isASCIIEncoded = BS.all (<= asciiDELETE)

{- |
Optimize `PDFHexString` into `PDFString`.
-}
optimizeString :: Logging m => PDFObject -> FallibleT m PDFObject
optimizeString object = case object of
  (PDFHexString values)
    | isASCIIEncoded encoded -> do
        -- If the hex string contains only ASCII characters, converts it to a
        -- PDFString which will be twice shorter.
        sayComparisonF
          (ctx object)
          "Hex string optimization (ASCII)"
          (BS.length values)
          (BS.length encoded)
        return $ PDFString encoded
    | isUTF16Encoded encoded -> case utf16beToPdfDocEncoding encoded of
        -- If the PDFHexString is UTF-16 but only contains PDFDocEncoding
        -- characters, converts it to a PDFString which will be four times
        -- shorter.
        Just pdfDocEncoded -> do
          sayComparisonF 
            (ctx object)
            "Hex string optimization (PDFDocEncoding)"
            (BS.length values)
            (BS.length pdfDocEncoded)
          return $ PDFString pdfDocEncoded
        Nothing -> do
          let utf16beEncoded = hexStringToString values
          sayComparisonF
            (ctx object)
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
          sayComparisonF
            (ctx object)
            "Hex string optimization (PDFDocEncoding)"
            (BS.length encoded)
            (BS.length pdfDocEncoded)
          return $ PDFString pdfDocEncoded
        Nothing -> return object
    | otherwise -> return object
  _anyOtherObject -> return object