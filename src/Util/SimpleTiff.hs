module Util.SimpleTiff (simpleTiff) where

import Data.ByteString (ByteString)
import Data.ByteString.Builder
    ( Builder
    , byteString
    , toLazyByteString
    , word16LE
    , word32LE
    )
import Data.ByteString.Lazy qualified as BL
import Data.ColorSpace
    ( ColorSpace (ColorSpaceCMYK, ColorSpaceGray, ColorSpaceRGB, ColorSpaceUnknown)
    , csComponents
    )
import Data.Kind (Type)
import Data.Word (Word16, Word32)

build :: Builder -> ByteString
build = BL.toStrict . toLazyByteString

simpleTiff :: Int -> Int -> ColorSpace -> ByteString -> ByteString
simpleTiff width height ColorSpaceGray imageData = build $
     tiffMagicLE
  <> word32LE 0x00000008 -- Offset of the first IFD
  -- Offset 0x00000008
  <> word16LE 12 -- Number of directory entries
  <> tiffImageWidth width
  <> tiffImageLength height
  <> tiffBitsPerSample ColorSpaceGray 0
  <> tiffNoCompression
  <> tiffGrayPhotometricInterpretation
  <> tiffStripOffsets 0x0000009E
  <> tiffGraySamplesPerPixel
  <> tiffRowsPerStrip height
  <> tiffStripByteCounts width height ColorSpaceGray
  <> tiffXResolution
  <> tiffYResolution
  <> tiffResolutionUnit
  <> word32LE 0x00000000 -- No more Image File Directory
  -- Offset 0x0000009E -- 8 + 2 + 12 * 12 + 4 = 158
  <> byteString imageData

simpleTiff width height ColorSpaceRGB imageData = build $
     tiffMagicLE
  <> word32LE 0x00000008 -- Offset of the first IFD
  -- Offset 0x00000008
  <> word16LE 12 -- Number of directory entries
  <> tiffImageWidth width
  <> tiffImageLength height
  <> tiffBitsPerSample ColorSpaceRGB 0x0000009E
  <> tiffNoCompression
  <> tiffRGBPhotometricInterpretation
  <> tiffStripOffsets 0x000000A4
  <> tiffRGBSamplesPerPixel
  <> tiffRowsPerStrip height
  <> tiffStripByteCounts width height ColorSpaceRGB
  <> tiffXResolution
  <> tiffYResolution
  <> tiffResolutionUnit
  <> word32LE 0x00000000 -- No more Image File Directory
  -- Offset 0x0000009E -- 8 + 2 + 12 * 12 + 4 = 158
  <> bitsPerSampleData ColorSpaceRGB
  -- Offset 0x000000A4 -- 158 + 6 = 164
  <> byteString imageData

simpleTiff width height ColorSpaceCMYK imageData = build $
     tiffMagicLE
  <> word32LE 0x00000008 -- Offset of the first IFD
  -- Offset 0x00000008
  <> word16LE 12 -- Number of directory entries
  <> tiffImageWidth width
  <> tiffImageLength height
  <> tiffBitsPerSample ColorSpaceCMYK 0x0000009E
  <> tiffNoCompression
  <> tiffCMYKPhotometricInterpretation
  <> tiffStripOffsets 0x000000A6
  <> tiffCMYKSamplesPerPixel
  <> tiffRowsPerStrip height
  <> tiffStripByteCounts width height ColorSpaceCMYK
  <> tiffXResolution
  <> tiffYResolution
  <> tiffResolutionUnit
  <> word32LE 0x00000000 -- No more Image File Directory
  -- Offset 0x0000009E -- 8 + 2 + 12 * 12 + 4 = 158
  <> bitsPerSampleData ColorSpaceCMYK
  -- Offset 0x000000A6 -- 158 + 8 = 166
  <> byteString imageData

simpleTiff _width _height (ColorSpaceUnknown _) _imageData = ""

type TagIdentifier :: Type
type TagIdentifier = Word16

type Offset :: Type
type Offset = Word32

type Components :: Type
type Components = [Int]

tagImageWidth, tagImageLength, tagBitsPerSample, tagCompression,
  tagPhotometricInterpretation, tagStripOffsets, tagSamplesPerPixel,
  tagRowsPerStrip, tagStripByteCounts, tagXResolution, tagYResolution,
  tagResolutionUnit :: TagIdentifier
tagImageWidth                 = 0x100
tagImageLength                = 0x101
tagBitsPerSample              = 0x102
tagCompression                = 0x103
tagPhotometricInterpretation  = 0x106
tagStripOffsets               = 0x111
tagSamplesPerPixel            = 0x115
tagRowsPerStrip               = 0x116
tagStripByteCounts            = 0x117
tagXResolution                = 0x11A
tagYResolution                = 0x11B
tagResolutionUnit             = 0x128

longTag :: TagIdentifier -> Int -> Builder
longTag tag value = word16LE tag
                 <> word16LE 4
                 <> word32LE 1
                 <> word32LE (fromIntegral value)

shortTag :: TagIdentifier -> Int -> Builder
shortTag tag value = word16LE tag
                  <> word16LE 3
                  <> word32LE 1
                  <> word32LE (fromIntegral value)

offsetShortTag :: TagIdentifier -> Int -> Offset -> Builder
offsetShortTag tag count offset = word16LE tag
                               <> word16LE 3
                               <> word32LE (fromIntegral count)
                               <> word32LE offset

components :: ColorSpace -> Components
components = flip replicate 8 . csComponents

tiffImageWidth :: Int -> Builder
tiffImageWidth = longTag tagImageWidth

tiffImageLength :: Int -> Builder
tiffImageLength = longTag tagImageLength

bitsPerSampleData :: ColorSpace -> Builder
bitsPerSampleData = foldMap (word16LE . fromIntegral) . components

tiffBitsPerSample :: ColorSpace -> Offset -> Builder
tiffBitsPerSample ColorSpaceGray _offset = shortTag tagBitsPerSample 8
tiffBitsPerSample colorSpace offset =
  offsetShortTag tagBitsPerSample (csComponents colorSpace) offset

tiffNoCompression :: Builder
tiffNoCompression = shortTag tagCompression 1

tiffStripOffsets :: Offset -> Builder
tiffStripOffsets = longTag tagStripOffsets . fromIntegral

tiffRGBPhotometricInterpretation :: Builder
tiffRGBPhotometricInterpretation = shortTag tagPhotometricInterpretation 2

tiffGrayPhotometricInterpretation :: Builder
tiffGrayPhotometricInterpretation = shortTag tagPhotometricInterpretation 1

tiffCMYKPhotometricInterpretation :: Builder
tiffCMYKPhotometricInterpretation = shortTag tagPhotometricInterpretation 5

tiffGraySamplesPerPixel :: Builder
tiffGraySamplesPerPixel = shortTag tagSamplesPerPixel 1

tiffRGBSamplesPerPixel :: Builder
tiffRGBSamplesPerPixel = shortTag tagSamplesPerPixel 3

tiffCMYKSamplesPerPixel :: Builder
tiffCMYKSamplesPerPixel = shortTag tagSamplesPerPixel 4

tiffRowsPerStrip :: Int -> Builder
tiffRowsPerStrip = longTag tagRowsPerStrip

tiffStripByteCounts :: Int -> Int -> ColorSpace -> Builder
tiffStripByteCounts width height colorSpace =
  longTag tagStripByteCounts (width * height * csComponents colorSpace)

tiffXResolution :: Builder
tiffXResolution = shortTag tagXResolution 72 -- 72 DPI

tiffYResolution :: Builder
tiffYResolution = shortTag tagYResolution 72 -- 72 DPI

tiffResolutionUnit :: Builder
tiffResolutionUnit = shortTag tagResolutionUnit 2 -- Inches

tiffMagicLE :: Builder
tiffMagicLE = word16LE 0x4949 <> word16LE 42
