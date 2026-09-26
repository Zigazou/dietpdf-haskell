-- | Conservative horizontal font metrics for text-position proofs.
-- Widths are indexed by character code (simple fonts) or CID (Identity-H),
-- never by Unicode or glyph ID. Unsupported encodings remain unknown.
module PDF.Graphics.TextMetrics
  ( FontMetrics
  , ExtFont (..)
  , TextResources (..)
  , buildTextResources
  , glyphWidths
  , serializedNumber
  ) where

import Control.Applicative ((<|>))
import Control.Monad (guard)

import Data.Binary (Word8)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Foldable (toList)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.PDF.PDFObject
  (PDFObject (PDFArray, PDFDictionary, PDFName, PDFNumber))

import Numeric (readFloat)

import PDF.Document.ResourceContext (resolve, value)
import PDF.Object.Object.Properties (getValueForKey)

import Util.Dictionary (Dictionary)
import Util.Number (fromNumber)

-- | Interpret the decimal actually written by the PDF serializer, rather than
-- the binary approximation held in a Double. Bound inputs before fromNumber's
-- fixed-precision Int conversion. Rewrites are checked again after
-- serialization.
serializedNumber :: Double -> Maybe Rational
serializedNumber n = do
  guard (not (isNaN n || isInfinite n) && abs n <= 1e9)
  let
    bytes :: String
    bytes = BSC.unpack (fromNumber n)

    sign :: Rational
    magnitude :: String
    (sign, magnitude) = case bytes of
      '-' : rest -> (-1, rest)
      _          -> (1, bytes)

    digits :: String
    digits = case magnitude of
      '.' : _ -> '0' : magnitude
      _       -> magnitude
  case readFloat digits of
    [(number, "")] -> Just (sign * number)
    _              -> Nothing

type FontMetrics :: Type
data FontMetrics
  = SimpleWidths !(IntMap Rational) !(Maybe Rational)
  | IdentityWidths !(IntMap (Int, Rational)) !Rational
  deriving stock (Eq, Show)

type ExtFont :: Type
data ExtFont = UnchangedFont | UnknownFont | SelectedFont !FontMetrics !Rational
  deriving stock (Eq, Show)

type TextResources :: Type
data TextResources = TextResources
  { textFonts    :: !(Dictionary FontMetrics)
  , textExtFonts :: !(Dictionary ExtFont)
  } deriving stock (Eq, Show)

-- | Decode each resource once, outside both scale trials and optimization
-- loops. Malformed widths, overlapping CID ranges and reference cycles are
-- rejected.
buildTextResources
  :: IntMap PDFObject
  -> Maybe (Dictionary PDFObject)
  -> TextResources
buildTextResources objects resources = TextResources
  (Map.mapMaybe font (category "Font"))
  (Map.map extFont (category "ExtGState"))
 where
  category :: ByteString -> Dictionary PDFObject
  category key = fromMaybe Map.empty $ do
    dict <- resources
    PDFDictionary entries <- Map.lookup key dict >>= resolve objects
    pure entries

  number :: PDFObject -> Maybe Rational
  number object = do
    PDFNumber n <- resolve objects object
    serializedNumber n

  integer :: Int -> PDFObject -> Maybe Int
  integer limit object = do
    n <- number object
    guard (n >= 0 && n <= fromIntegral limit && denominatorIsOne n)
    pure (round n)

  denominatorIsOne :: Rational -> Bool
  denominatorIsOne n = n == fromInteger (round n)

  defaultNumber :: ByteString -> Rational -> PDFObject -> Maybe Rational
  defaultNumber key fallback object = case getValueForKey key object of
    Nothing    -> Just fallback
    Just entry -> number entry

  font :: PDFObject -> Maybe FontMetrics
  font entry = do
    object <- resolve objects entry
    subtype <- value objects "Subtype" object
    case subtype of
      PDFName name | name `elem` ["Type1", "MMType1", "TrueType"] -> do
        first <- getValueForKey "FirstChar" object >>= integer 255
        lastCode <- getValueForKey "LastChar" object >>= integer 255
        PDFArray entries <- value objects "Widths" object
        guard (first <= lastCode && length entries == lastCode - first + 1)
        widths <- traverse number (toList entries)

        let
          missing :: Maybe Rational
          missing = do
              descriptor@(PDFDictionary _) <- value objects
                                                    "FontDescriptor"
                                                    object

              defaultNumber "MissingWidth" 0 descriptor

        pure (SimpleWidths (IM.fromDistinctAscList (zip [first ..] widths))
                           missing
             )

      PDFName "Type0" -> do
        guard (value objects "Encoding" object == Just (PDFName "Identity-H"))

        PDFArray descendants <- value objects "DescendantFonts" object
        [descendant] <- pure (toList descendants)
        cid <- resolve objects descendant

        guard (value objects "Subtype" cid `elem`
          [Just (PDFName "CIDFontType0"), Just (PDFName "CIDFontType2")])
        dw <- defaultNumber "DW" 1000 cid

        widths <- case getValueForKey "W" cid of
          Nothing -> Just IM.empty
          Just w -> do
            PDFArray entries <- resolve objects w
            ranges <- cidRanges (toList entries)
            -- Check intervals without expanding potentially large ranges.
            guard (and (zipWith (\(_, end, _) (start, _, _) -> end < start)
                                ranges (drop 1 ranges)))
            pure (IM.fromDistinctAscList [ (start, (end, width))
                                         | (start, end, width) <- ranges
                                         ]
                 )

        pure (IdentityWidths widths dw)

      _ -> Nothing

  cidRanges :: [PDFObject] -> Maybe [(Int, Int, Rational)]
  cidRanges [] = Just []
  cidRanges (startObject : next : rest) = do
    start <- integer 65535 startObject
    resolved <- resolve objects next
    case resolved of
      PDFArray entries -> do
        guard (not (null entries) && length entries <= 65536 - start)
        widths <- traverse number (toList entries)
        more <- cidRanges rest
        pure (zipWith (\code width -> (code, code, width))
                      [start ..]
                      widths
              ++ more
             )

      _ -> do
        end <- integer 65535 resolved
        guard (end >= start)

        case rest of
          widthObject : remaining -> do
            width <- number widthObject
            more <- cidRanges remaining
            pure ((start, end, width) : more)

          _ -> Nothing

  cidRanges _ = Nothing

  extFont :: PDFObject -> ExtFont
  extFont entry = fromMaybe UnknownFont $ do
    object <- resolve objects entry
    PDFDictionary _ <- pure object
    case getValueForKey "Font" object of
      Nothing -> Just UnchangedFont
      Just setting -> do
        PDFArray entries <- resolve objects setting
        [fontObject, sizeObject] <- pure (toList entries)
        SelectedFont <$> font fontObject <*> number sizeObject

-- | Width in thousandths of text space and eligibility for word spacing.
-- Identity-H consumes two bytes per CID; even CID 32 is not a single-byte
-- space.
glyphWidths :: FontMetrics -> ByteString -> Maybe [(Rational, Bool)]
glyphWidths (SimpleWidths widths missing) bytes =
  traverse glyph (BS.unpack bytes)
 where
  glyph code = do
    width <- IM.lookup (fromIntegral code) widths <|> missing
    pure (width, code == 32)

glyphWidths (IdentityWidths widths dw) bytes = pairs (BS.unpack bytes)
 where
  pairs :: [Word8] -> Maybe [(Rational, Bool)]
  pairs [] = Just []

  pairs (hi : lo : rest) = do
    let code = fromIntegral hi * 256 + fromIntegral lo
        width = case IM.lookupLE code widths of
          Just (_, (end, w)) | code <= end -> w
          _                                -> dw
    ((width, False) :) <$> pairs rest

  pairs _ = Nothing
