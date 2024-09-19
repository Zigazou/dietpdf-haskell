module Jpeg.Marker
( Marker (SOF, JPG, DHT, DAC, SOI, EOI, SOS, DQT, DNL, DRI, DHP, EXP, APP, COM, TEM, RST, RES)
, markerP
, hasData
, encode
) where

import Data.Binary (Get)
import Data.Binary.Get (label)
import Data.Binary.Parser (satisfy, takeWhile1)
import Data.ByteString qualified as BS
import Data.Kind (Type)
import Data.Word (Word8)

type Marker :: Type
data Marker
  = SOF !Word8  -- ^ Start of Frame
  | DHT         -- ^ Define Huffman table
  | DAC         -- ^ Define arithmetic coding conditioning
  | RST !Word8  -- ^ Restart
  | SOI         -- ^ Start of image
  | EOI         -- ^ End of image
  | SOS         -- ^ Start of scan
  | DQT         -- ^ Define quantization table
  | DNL         -- ^ Define number of lines
  | DRI         -- ^ Define restart interval
  | DHP         -- ^ Define hierarchical progression
  | EXP         -- ^ Expand reference component
  | APP !Word8  -- ^ Application segment
  | JPG !Word8  -- ^ Reserved for JPEG extensions
  | COM         -- ^ Comment
  | TEM         -- ^ For temporary private use in arithmetic coding
  | RES !Word8  -- ^ Reserved
  deriving stock (Eq, Show)

identifyMarker :: Word8 -> Maybe Marker
identifyMarker 0xC4 = Just DHT
identifyMarker 0xCC = Just DAC
identifyMarker 0xD8 = Just SOI
identifyMarker 0xD9 = Just EOI
identifyMarker 0xDA = Just SOS
identifyMarker 0xDB = Just DQT
identifyMarker 0xDC = Just DNL
identifyMarker 0xDD = Just DRI
identifyMarker 0xDE = Just DHP
identifyMarker 0xDF = Just EXP
identifyMarker 0xFE = Just COM
identifyMarker 0x01 = Just TEM
identifyMarker byte
  | byte >= 0xC0 && byte <= 0xCF = Just (SOF (byte - 0xC0))
  | byte >= 0xE0 && byte <= 0xEF = Just (APP (byte - 0xE0))
  | byte == 0xC8                 = Just (JPG byte)
  | byte >= 0xF0 && byte <= 0xFD = Just (JPG byte)
  | byte >= 0x02 && byte <= 0xBF = Just (RES byte)
  | byte >= 0xD0 && byte <= 0xD7 = Just (RST (byte - 0xD0))
  | otherwise = Nothing

encode :: Marker -> BS.ByteString
encode (SOF n) = BS.pack [0xFF, 0xC0 + n]
encode DHT     = BS.pack [0xFF, 0xC4]
encode DAC     = BS.pack [0xFF, 0xCC]
encode (RST n) = BS.pack [0xFF, 0xD0 + n]
encode SOI     = BS.pack [0xFF, 0xD8]
encode EOI     = BS.pack [0xFF, 0xD9]
encode SOS     = BS.pack [0xFF, 0xDA]
encode DQT     = BS.pack [0xFF, 0xDB]
encode DNL     = BS.pack [0xFF, 0xDC]
encode DRI     = BS.pack [0xFF, 0xDD]
encode DHP     = BS.pack [0xFF, 0xDE]
encode EXP     = BS.pack [0xFF, 0xDF]
encode (APP n) = BS.pack [0xFF, 0xE0 + n]
encode (JPG n) = BS.pack [0xFF, n]
encode COM     = BS.pack [0xFF, 0xFE]
encode TEM     = BS.pack [0xFF, 0x01]
encode (RES n) = BS.pack [0xFF, n]

hasData :: Marker -> Bool
hasData SOI     = False
hasData EOI     = False
hasData (RST _) = False
hasData _       = True

markerP :: Get Marker
markerP = label "marker" $ do
  _markerStart <- takeWhile1 (== 0xFF)
  markerId <- satisfy (\x -> x /= 0x00 && x /= 0xFF)
  case identifyMarker markerId of
    Just marker -> return marker
    Nothing     -> fail "unknown marker"
