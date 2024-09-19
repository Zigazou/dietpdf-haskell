module Jpeg.Segment
( Segment (StartOfFrame, DefineHuffmanTable, DefineArithmeticCoding, Restart, StartOfImage, EndOfImage, StartOfScan, DefineQuantizationTable, DefineNumberOfLines, DefineRestartInterval, DefineHierarchicalProgression, ExpandReferenceComponent, Application, JpegExtension, Comment, Temporary, Reserved)
, segmentP
, encode
) where

import Control.Applicative (Alternative (many), (<|>))

import Data.Binary.Get (Get, getByteString, getWord16be)
import Data.Binary.Parser (label, satisfy)
import Data.Binary.Parser.Word8 (word8)
import Data.ByteString qualified as BS
import Data.Kind (Type)
import Data.Word (Word8)

import Jpeg.Marker
    ( Marker (APP, COM, DAC, DHP, DHT, DNL, DQT, DRI, EOI, EXP, JPG, RES, RST, SOF, SOI, SOS, TEM)
    , markerP
    )
import Jpeg.Marker qualified as Marker (encode)

type Segment :: Type
data Segment
  = StartOfFrame !Word8 !BS.ByteString
  | DefineHuffmanTable !BS.ByteString
  | DefineArithmeticCoding !BS.ByteString
  | Restart !Word8
  | StartOfImage
  | EndOfImage
  | StartOfScan !BS.ByteString !BS.ByteString
  | DefineQuantizationTable !BS.ByteString
  | DefineNumberOfLines !BS.ByteString !BS.ByteString
  | DefineRestartInterval !BS.ByteString !BS.ByteString
  | DefineHierarchicalProgression !BS.ByteString !BS.ByteString
  | ExpandReferenceComponent !BS.ByteString !BS.ByteString
  | Application !Word8 !BS.ByteString !BS.ByteString
  | JpegExtension !Word8 !BS.ByteString !BS.ByteString
  | Comment !BS.ByteString !BS.ByteString
  | Temporary !BS.ByteString !BS.ByteString
  | Reserved !Word8 !BS.ByteString !BS.ByteString
  deriving stock (Eq, Show)

encodeHeader :: BS.ByteString -> BS.ByteString
encodeHeader h = BS.pack [hiByte, loByte] <> h
  where
    headerLength :: Int
    headerLength = BS.length h + 2

    hiByte :: Word8
    hiByte = fromIntegral (headerLength `div` 256)

    loByte :: Word8
    loByte = fromIntegral (headerLength `mod` 256)

-- Insert 0x00 after each 0xFF byte in the data
encodeData :: BS.ByteString -> BS.ByteString
encodeData = BS.concatMap (\b -> if b == 0xFF
                                    then BS.pack [0xFF, 0x00]
                                    else BS.singleton b)

encode :: Segment -> BS.ByteString
encode (StartOfFrame n h)                  = Marker.encode (SOF n) <> encodeHeader h
encode (DefineHuffmanTable h)              = Marker.encode DHT <> encodeHeader h
encode (DefineArithmeticCoding h)          = Marker.encode DAC <> encodeHeader h
encode (Restart n)                         = Marker.encode (RST n)
encode StartOfImage                        = Marker.encode SOI
encode EndOfImage                          = Marker.encode EOI
encode (StartOfScan h d)                   = Marker.encode SOS <> encodeHeader h <> encodeData d
encode (DefineQuantizationTable h)         = Marker.encode DQT <> encodeHeader h
encode (DefineNumberOfLines h d)           = Marker.encode DNL <> encodeHeader h <> encodeData d
encode (DefineRestartInterval h d)         = Marker.encode DRI <> encodeHeader h <> encodeData d
encode (DefineHierarchicalProgression h d) = Marker.encode DHP <> encodeHeader h <> encodeData d
encode (ExpandReferenceComponent h d)      = Marker.encode EXP <> encodeHeader h <> encodeData d
encode (Application n h d)                 = Marker.encode (APP n) <> encodeHeader h <> encodeData d
encode _anyOtherSegment = ""
-- encode (JpegExtension n h d)               = Marker.encode (JPG n) <> encodeHeader h <> encodeData d
-- encode (Comment h d)                       = Marker.encode COM <> encodeHeader h <> encodeData d
-- encode (Temporary h d)                     = Marker.encode TEM <> encodeHeader h <> encodeData d
-- encode (Reserved n h d)                    = Marker.encode (RES n) <> encodeHeader h <> encodeData d

segmentDataP :: Get BS.ByteString
segmentDataP = do
  BS.pack <$> many (noffByte <|> ffByte)
 where
    noffByte :: Get Word8
    noffByte = satisfy (/= 0xFF)

    ffByte :: Get Word8
    ffByte = word8 0xFF >> word8 0x00 >> return 0xFF

headerP :: Get BS.ByteString
headerP = do
  headerLength <- getWord16be
  getByteString (fromIntegral headerLength - 2)

segmentP :: Get Segment
segmentP = label "segment" $ do
  markerP >>= \case
    SOI   -> return StartOfImage
    EOI   -> return EndOfImage
    RST n -> return (Restart n)
    SOF n -> StartOfFrame n                 <$> headerP
    DHT   -> DefineHuffmanTable             <$> headerP
    DAC   -> DefineArithmeticCoding         <$> headerP
    SOS   -> StartOfScan                    <$> headerP <*> segmentDataP
    DQT   -> DefineQuantizationTable        <$> headerP
    DNL   -> DefineNumberOfLines            <$> headerP <*> segmentDataP
    DRI   -> DefineRestartInterval          <$> headerP <*> segmentDataP
    DHP   -> DefineHierarchicalProgression  <$> headerP <*> segmentDataP
    EXP   -> ExpandReferenceComponent       <$> headerP <*> segmentDataP
    APP n -> Application n                  <$> headerP <*> segmentDataP
    JPG n -> JpegExtension n                <$> headerP <*> segmentDataP
    COM   -> Comment                        <$> headerP <*> segmentDataP
    TEM   -> Temporary                      <$> headerP <*> segmentDataP
    RES n -> Reserved n                     <$> headerP <*> segmentDataP
