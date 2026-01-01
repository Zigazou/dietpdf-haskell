{-|
Minimal TIFF encoder for raw pixel data.

This module provides a very small TIFF writer used to wrap raw pixel bytes into
an uncompressed, single-image TIFF container.

The implementation is intentionally minimal and currently:

- writes Little Endian TIFF ("II")
- writes a single Image File Directory (IFD)
- stores the pixel data as a single strip
- supports 8-bit samples for grayscale, RGB, and CMYK

Unsupported color spaces are encoded as an empty 'ByteString'.
-}
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

{-|
Build a minimal uncompressed TIFF image.

Parameters:
- @width@: image width in pixels
- @height@: image height in pixels
- @colorSpace@: supported values are grayscale, RGB, and CMYK
- @imageData@: raw pixel bytes (written as a single strip)

This function does not validate that @imageData@ has the expected length (@width
* height * components@).

For unsupported color spaces, this function returns the empty string.
-}
simpleTiff :: Int -> Int -> ColorSpace -> ByteString -> ByteString
simpleTiff width height ColorSpaceGray imageData = build $
     tiffMagicLE
  {- Offset of the first IFD -}
  <> word32LE 0x00000008
  {- Offset 0x00000008 -}
  {- Number of directory entries -}
  <> word16LE 12
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
  {- No more Image File Directory -}
  <> word32LE 0x00000000
  {- Offset 0x0000009E (8 + 2 + 12 * 12 + 4 = 158) -}
  <> byteString imageData

simpleTiff width height ColorSpaceRGB imageData = build $
     tiffMagicLE
  {- Offset of the first IFD -}
  <> word32LE 0x00000008
  {- Offset 0x00000008 -}
  {- Number of directory entries -}
  <> word16LE 12
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
  {- No more Image File Directory -}
  <> word32LE 0x00000000
  {- Offset 0x0000009E (8 + 2 + 12 * 12 + 4 = 158) -}
  <> bitsPerSampleData ColorSpaceRGB
  {- Offset 0x000000A4 (158 + 6 = 164) -}
  <> byteString imageData

simpleTiff width height ColorSpaceCMYK imageData = build $
     tiffMagicLE
  {- Offset of the first IFD -}
  <> word32LE 0x00000008
  {- Offset 0x00000008 -}
  {- Number of directory entries -}
  <> word16LE 12
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
  {- No more Image File Directory -}
  <> word32LE 0x00000000
  {- Offset 0x0000009E (8 + 2 + 12 * 12 + 4 = 158) -}
  <> bitsPerSampleData ColorSpaceCMYK
  {- Offset 0x000000A6 (158 + 8 = 166) -}
  <> byteString imageData

simpleTiff _width _height (ColorSpaceUnknown _) _imageData = ""

{-|
Tag identifier for TIFF fields.
-}
type TagIdentifier :: Type
type TagIdentifier = Word16

{-|
Offset type for TIFF fields.
-}
type Offset :: Type
type Offset = Word32

{-|
Components type for TIFF color spaces.
-}
type Components :: Type
type Components = [Int]

{-|
TIFF tag identifiers used by this encoder.
-}
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

{-|
Helper to build a TIFF tag with a long (4-byte) value.
-}
longTag :: TagIdentifier -> Int -> Builder
longTag tag value = word16LE tag
                 <> word16LE 4
                 <> word32LE 1
                 <> word32LE (fromIntegral value)

{-|
Helper to build a TIFF tag with a short (2-byte) value.
-}
shortTag :: TagIdentifier -> Int -> Builder
shortTag tag value = word16LE tag
                  <> word16LE 3
                  <> word32LE 1
                  <> word32LE (fromIntegral value)

{-|
Helper to build a TIFF tag with a short (2-byte) value stored at an offset.
-}
offsetShortTag :: TagIdentifier -> Int -> Offset -> Builder
offsetShortTag tag count offset = word16LE tag
                               <> word16LE 3
                               <> word32LE (fromIntegral count)
                               <> word32LE offset

{-|
Retrieve the components for a given color space.
-}
components :: ColorSpace -> Components
components = flip replicate 8 . csComponents

{-|
TIFF ImageWidth field.
-}
tiffImageWidth :: Int -> Builder
tiffImageWidth = longTag tagImageWidth

{-|
TIFF ImageLength field.
-}
tiffImageLength :: Int -> Builder
tiffImageLength = longTag tagImageLength

{-|
TIFF BitsPerSample field data.
-}
bitsPerSampleData :: ColorSpace -> Builder
bitsPerSampleData = foldMap (word16LE . fromIntegral) . components

{-|
TIFF BitsPerSample field.
-}
tiffBitsPerSample :: ColorSpace -> Offset -> Builder
tiffBitsPerSample ColorSpaceGray _offset = shortTag tagBitsPerSample 8
tiffBitsPerSample colorSpace offset =
  offsetShortTag tagBitsPerSample (csComponents colorSpace) offset

{-|
TIFF Compression field (no compression).
-}
tiffNoCompression :: Builder
tiffNoCompression = shortTag tagCompression 1

{-|
TIFF StripOffsets field.
-}
tiffStripOffsets :: Offset -> Builder
tiffStripOffsets = longTag tagStripOffsets . fromIntegral

{-|
TIFF PhotometricInterpretation field for RGB color space.
-}
tiffRGBPhotometricInterpretation :: Builder
tiffRGBPhotometricInterpretation = shortTag tagPhotometricInterpretation 2

{-|
TIFF PhotometricInterpretation field for Grayscale color space.
-}
tiffGrayPhotometricInterpretation :: Builder
tiffGrayPhotometricInterpretation = shortTag tagPhotometricInterpretation 1

{-|
TIFF PhotometricInterpretation field for CMYK color space.
-}
tiffCMYKPhotometricInterpretation :: Builder
tiffCMYKPhotometricInterpretation = shortTag tagPhotometricInterpretation 5

{-|
TIFF SamplesPerPixel field for Grayscale color space.
-}
tiffGraySamplesPerPixel :: Builder
tiffGraySamplesPerPixel = shortTag tagSamplesPerPixel 1

{-|
TIFF SamplesPerPixel field for RGB color space.
-}
tiffRGBSamplesPerPixel :: Builder
tiffRGBSamplesPerPixel = shortTag tagSamplesPerPixel 3

{-|
TIFF SamplesPerPixel field for CMYK color space.
-}
tiffCMYKSamplesPerPixel :: Builder
tiffCMYKSamplesPerPixel = shortTag tagSamplesPerPixel 4

{-|
TIFF RowsPerStrip field.
-}
tiffRowsPerStrip :: Int -> Builder
tiffRowsPerStrip = longTag tagRowsPerStrip

{-|
TIFF StripByteCounts field.
-}
tiffStripByteCounts :: Int -> Int -> ColorSpace -> Builder
tiffStripByteCounts width height colorSpace =
  longTag tagStripByteCounts (width * height * csComponents colorSpace)

{-|
Fixed X resolution field written by this encoder.

The value is set to 72 DPI.
-}
tiffXResolution :: Builder
tiffXResolution = shortTag tagXResolution 72

{-|
Fixed Y resolution field written by this encoder.

The value is set to 72 DPI.
-}
tiffYResolution :: Builder
tiffYResolution = shortTag tagYResolution 72

{-|
Resolution unit written by this encoder.

The value @2@ denotes inches.
-}
tiffResolutionUnit :: Builder
tiffResolutionUnit = shortTag tagResolutionUnit 2

{-|
TIFF magic number for Little Endian files.
-}
tiffMagicLE :: Builder
tiffMagicLE = word16LE 0x4949 <> word16LE 42
