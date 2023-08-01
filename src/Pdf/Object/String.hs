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
import           Util.Ascii                     ( asciiNUL )
import           Util.UnifiedError              ( FallibleT )
import           Util.Logging                   ( Logging
                                                , sayComparisonF
                                                )

utf16beBOM :: BS.ByteString
utf16beBOM = "\xfe\xff"

utf16beToLatin1 :: BS.ByteString -> Maybe BS.ByteString
utf16beToLatin1 utf16beString =
  snd <$> BS.foldl' convert (Just (0, "")) utf16beString
 where
  convert :: Maybe (Int, BS.ByteString) -> Word8 -> Maybe (Int, BS.ByteString)
  convert Nothing       _    = Nothing
  convert (Just (0, _)) 0xFE = Just (1, "")
  convert (Just (0, _)) _    = Nothing
  convert (Just (1, _)) 0xFF = Just (2, "")
  convert (Just (1, _)) _    = Nothing
  convert (Just (offset, current)) byte
    | even offset && byte == asciiNUL = Just (offset + 1, current)
    | odd offset && byte > asciiNUL   = Just (offset + 1, BS.snoc current byte)
    | otherwise                       = Nothing

isUTF16Encoded :: BS.ByteString -> Bool
isUTF16Encoded string | BS.length string < 2           = False
                      | BS.take 2 string /= utf16beBOM = False
                      | otherwise                      = True

isLatin1Encoded :: BS.ByteString -> Bool
isLatin1Encoded = BS.all (> asciiNUL)

{- |
Optimize `PDFHexString` into `PDFString`.
-}
optimizeString :: Logging m => PDFObject -> FallibleT m PDFObject
optimizeString object = case object of
  (PDFHexString values)
    |
      -- If the hex string contains only ASCII characters, converts it to a
      -- PDFString which will be twice shorter.
      isLatin1Encoded encoded -> do
      sayComparisonF "Hex string optimization"
                     (BS.length values)
                     (BS.length encoded)
      return $ PDFString encoded
    |
      -- If the PDFHexString is UTF-16 but only contains ASCII characters,
      -- converts it to a PDFString which will be four times shorter.
      isUTF16Encoded encoded -> case utf16beToLatin1 encoded of
      Just asciiEncoded -> do
        sayComparisonF "Hex string optimization"
                       (BS.length values)
                       (BS.length asciiEncoded)
        return $ PDFString asciiEncoded
      Nothing -> return object
    | otherwise -> return object
    where encoded = hexStringToString values
  _anyOtherObject -> return object
