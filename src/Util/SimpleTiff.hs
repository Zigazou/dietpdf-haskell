module Util.SimpleTiff (simpleTiff) where

import Data.ByteString qualified as BS
import Data.ByteString.Builder
    ( Builder
    , byteString
    , toLazyByteString
    , word16LE
    , word32LE
    )
import Data.ByteString.Lazy qualified as BL
import Data.Kind (Type)
import Data.Word (Word16, Word32)

build :: Builder -> BS.ByteString
build = BL.toStrict . toLazyByteString

simpleTiff :: Int -> Int -> Int -> BS.ByteString -> BS.ByteString
simpleTiff width height 1 imageData = build $
     tiffMagicLE
  <> word32LE 0x00000008 -- Offset of the first IFD
  -- Offset 0x00000008
  <> word16LE 12 -- Number of directory entries
  <> tiffImageWidth width
  <> tiffImageLength height
  <> tiffBitsPerSample grayComponents 0
  <> tiffNoCompression
  <> tiffGrayPhotometricInterpretation
  <> tiffStripOffsets 0x0000009E
  <> tiffGraySamplesPerPixel
  <> tiffRowsPerStrip height
  <> tiffStripByteCounts width height grayComponents
  <> tiffXResolution
  <> tiffYResolution
  <> tiffResolutionUnit
  <> word32LE 0x00000000 -- No more Image File Directory
  -- Offset 0x0000009E -- 8 + 2 + 12 * 12 + 4 = 158
  <> byteString imageData

simpleTiff width height 3 imageData = build $
     tiffMagicLE
  <> word32LE 0x00000008 -- Offset of the first IFD
  -- Offset 0x00000008
  <> word16LE 12 -- Number of directory entries
  <> tiffImageWidth width
  <> tiffImageLength height
  <> tiffBitsPerSample rgbComponents 0x0000009E
  <> tiffNoCompression
  <> tiffRGBPhotometricInterpretation
  <> tiffStripOffsets 0x000000A4
  <> tiffRGBSamplesPerPixel
  <> tiffRowsPerStrip height
  <> tiffStripByteCounts width height rgbComponents
  <> tiffXResolution
  <> tiffYResolution
  <> tiffResolutionUnit
  <> word32LE 0x00000000 -- No more Image File Directory
  -- Offset 0x0000009E -- 8 + 2 + 12 * 12 + 4 = 158
  <> bitsPerSampleData rgbComponents
  -- Offset 0x000000A4 -- 158 + 6 = 164
  <> byteString imageData

simpleTiff width height 4 imageData = build $
     tiffMagicLE
  <> word32LE 0x00000008 -- Offset of the first IFD
  -- Offset 0x00000008
  <> word16LE 12 -- Number of directory entries
  <> tiffImageWidth width
  <> tiffImageLength height
  <> tiffBitsPerSample cmykComponents 0x0000009E
  <> tiffNoCompression
  <> tiffCMYKPhotometricInterpretation
  <> tiffStripOffsets 0x000000A6
  <> tiffCMYKSamplesPerPixel
  <> tiffRowsPerStrip height
  <> tiffStripByteCounts width height cmykComponents
  <> tiffXResolution
  <> tiffYResolution
  <> tiffResolutionUnit
  <> word32LE 0x00000000 -- No more Image File Directory
  -- Offset 0x0000009E -- 8 + 2 + 12 * 12 + 4 = 158
  <> bitsPerSampleData cmykComponents
  -- Offset 0x000000A6 -- 158 + 8 = 166
  <> byteString imageData

simpleTiff _width _height _components _imageData = ""

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

cmykComponents :: Components
cmykComponents = [8, 8, 8, 8]

rgbComponents :: Components
rgbComponents = [8, 8, 8]

grayComponents :: Components
grayComponents = [8]

tiffImageWidth :: Int -> Builder
tiffImageWidth = longTag tagImageWidth

tiffImageLength :: Int -> Builder
tiffImageLength = longTag tagImageLength

bitsPerSampleData :: Components -> Builder
bitsPerSampleData = foldMap (word16LE . fromIntegral)

tiffBitsPerSample :: Components -> Offset -> Builder
tiffBitsPerSample [component] _offset = shortTag tagBitsPerSample component
tiffBitsPerSample components offset =
  offsetShortTag tagBitsPerSample (length components) offset

tiffNoCompression :: Builder
tiffNoCompression = shortTag tagCompression 1

tiffStripOffsets :: Offset -> Builder
tiffStripOffsets = longTag tagStripOffsets . fromIntegral

tiffRGBPhotometricInterpretation :: Builder
tiffRGBPhotometricInterpretation = shortTag tagPhotometricInterpretation 2

tiffGrayPhotometricInterpretation :: Builder
tiffGrayPhotometricInterpretation = shortTag tagPhotometricInterpretation 0

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

tiffStripByteCounts :: Int -> Int -> Components -> Builder
tiffStripByteCounts width height components =
  longTag tagStripByteCounts (width * height * length components)

tiffXResolution :: Builder
tiffXResolution = shortTag tagXResolution 72 -- 72 DPI

tiffYResolution :: Builder
tiffYResolution = shortTag tagYResolution 72 -- 72 DPI

tiffResolutionUnit :: Builder
tiffResolutionUnit = shortTag tagResolutionUnit 2 -- Inches

tiffMagicLE :: Builder
tiffMagicLE = word16LE 0x4949 <> word16LE 42
