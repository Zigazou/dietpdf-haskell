{-# LANGUAGE OverloadedStrings #-}

module Pdf.Object.String
  ( optimizeString
  ) where

import qualified Data.ByteString               as BS
import           Data.Word                      ( Word8 )
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFHexString
                                                  , PDFString
                                                  )
                                                )
import           Util.String                    ( hexStringToString )
import           Util.Ascii                     ( asciiNUL
                                                , asciiDELETE
                                                )

utf16beBOM :: BS.ByteString
utf16beBOM = "\xfe\xff"

utf16beToAscii :: BS.ByteString -> Maybe BS.ByteString
utf16beToAscii utf16beString =
  snd <$> BS.foldl' convert (Just (0, "")) utf16beString
 where
  convert :: Maybe (Int, BS.ByteString) -> Word8 -> Maybe (Int, BS.ByteString)
  convert Nothing       _    = Nothing
  convert (Just (0, _)) 0xFE = Just (1, "")
  convert (Just (0, _)) _    = Nothing
  convert (Just (1, _)) 0xFF = Just (2, "")
  convert (Just (1, _)) _    = Nothing
  convert (Just (offset, current)) byte
    | even offset && byte == 0x00      = Just (offset + 1, current)
    | odd offset && byte < asciiDELETE = Just (offset + 1, BS.snoc current byte)
    | otherwise                        = Nothing

isUTF16Encoded :: BS.ByteString -> Bool
isUTF16Encoded string | BS.length string < 2           = False
                      | BS.take 2 string /= utf16beBOM = False
                      | otherwise                      = True

isAsciiEncoded :: BS.ByteString -> Bool
isAsciiEncoded = BS.all (\char -> char > asciiNUL && char < asciiDELETE)

{- |
Optimize `PDFHexString` into `PDFString`.
-}
optimizeString :: PDFObject -> PDFObject
optimizeString object@(PDFHexString values)
    -- If the hex string contains only ASCII characters, converts it to a
    -- PDFString which will be twice shorter.
  | isAsciiEncoded encoded = PDFString encoded
    -- If the PDFHexString is UTF-16 but only contains ASCII characters,
    -- converts it to a PDFString which will be four times shorter.
  | isUTF16Encoded encoded = maybe object PDFString (utf16beToAscii encoded)
    -- Otherwise, do nothing on a PDFHexString
  | otherwise = object
  where encoded = hexStringToString values
optimizeString object = object
