{- |
Zero-fill pixels hidden by a soft mask.

When an Image XObject carries an @/SMask@ that makes some of its pixels
exactly fully transparent (soft mask sample equal to @0@, i.e. alpha strictly
null), the colour values of those pixels never contribute to the rendered
page. Replacing them with a constant (zero) value does not change the visual
result but often makes the stream far more compressible, since it turns
irregular photographic noise into large uniform runs.

This is a "visually lossless" transformation in the sense described in the
DietPDF design notes: it assumes a straightforward alpha-zero compositing
model and does not attempt to reason about @/Interpolate@ neighbour bleeding.
The soft mask itself is left untouched.

Scope of this first implementation, kept deliberately narrow for safety:

* Only plain (single @FlateDecode@ filter, no other filters) 8-bit images are
  handled; JPEG\/JPEG2000\/CCITT streams are never decoded/re-encoded here.
* The image's colour space must be a literal @DeviceGray@\/@DeviceRGB@\/
  @DeviceCMYK@ (no @Indexed@ or @ICCBased@, to avoid guessing the component
  count).
* Neither the image nor the mask may declare a @/Decode@ array (the default
  decode is assumed).
* The image must have exactly one of @/SMask@ (an @/Mask@ image or colour-key
  mask disqualifies it).
* The mask must have the same @/Width@ and @/Height@ as the image (no
  resampling is attempted).

Cropping the image to the mask's opaque bounding box is a natural follow-up,
but requires rewriting the content stream's @cm@/@Do@ pairs (and proving the
XObject is not reused elsewhere with a different placement), which is a
separate piece of infrastructure and is not implemented here.
-}
module PDF.Document.ZeroFillMaskedImages
  ( zeroFillMaskedImages
  ) where

import Codec.Compression.Flate qualified as Flate
import Codec.Compression.Predict (Predictor (TIFFNoPrediction), unpredict)
import Codec.Compression.Predict.Predictor (decodePredictor)

import Data.Bitmap.BitmapConfiguration (BitmapConfiguration (BitmapConfiguration))
import Data.Bitmap.BitsPerComponent (BitsPerComponent (BC8Bits))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.Either (fromRight)
import Data.Fallible (Fallible)
import Data.Foldable (toList)
import Data.Kind (Type)
import Data.Logging (Logging)
import Data.PDF.Filter (Filter (Filter))
import Data.PDF.FilterList (mkFilterList)
import Data.PDF.PDFObject
  ( PDFObject (PDFIndirectObjectWithStream, PDFName, PDFNull, PDFNumber, PDFReference)
  )
import Data.PDF.PDFWork (PDFWork, getReference, sayComparisonP)
import Data.Word (Word8)

import Foreign.Ptr (Ptr)
import Foreign.Storable (pokeByteOff)

import PDF.Object.Container (getFilters, setFilters)
import PDF.Object.Object.Properties (getValueForKey)
import PDF.Object.State (getStream, getValue, setStream)

{-|
Geometry and fully decoded (unfiltered, unpredicted) raw pixel bytes of a
plain bitmap.
-}
type PlainBitmap :: Type
data PlainBitmap = PlainBitmap
  { pbWidth  :: !Int
  , pbHeight :: !Int
  , pbRaw    :: !ByteString
  }

{-|
Number of colour components implied by a literal device colour space.
-}
componentsOf :: PDFObject -> Maybe Int
componentsOf (PDFName "DeviceGray") = Just 1
componentsOf (PDFName "DeviceRGB" ) = Just 3
componentsOf (PDFName "DeviceCMYK") = Just 4
componentsOf _anyOtherColorSpace    = Nothing

{-|
The `Predictor` declared in a filter's decode parameters, defaulting to no
prediction (PDF predictor code 1) when absent or invalid.
-}
predictorOf :: PDFObject -> Predictor
predictorOf parms = case getValueForKey "Predictor" parms of
  Just (PDFNumber value) ->
    fromRight TIFFNoPrediction (decodePredictor (round value))
  _anyOtherValue -> TIFFNoPrediction

{-|
Decompress and un-predict a stream, given its bitmap configuration and
predictor.
-}
decodeRawBitmap
  :: BitmapConfiguration -> Predictor -> ByteString -> Fallible ByteString
decodeRawBitmap bitmapConfig predictor compressed =
  Flate.decompress compressed >>= unpredict predictor bitmapConfig

{-|
Try to read a plain bitmap (single FlateDecode filter, 8 bits per component,
no @/Decode@ array) with the given component count.

Returns 'Nothing' when the object does not match this narrow shape, or when
decoding fails, or when the decoded size is inconsistent with @/Width@,
@/Height@ and the component count.
-}
plainBitmap :: Logging m => Int -> PDFObject -> PDFWork m (Maybe PlainBitmap)
plainBitmap components object = do
  mWidth  <- getValue "Width" object
  mHeight <- getValue "Height" object
  mBpc    <- getValue "BitsPerComponent" object
  mDecode <- getValue "Decode" object
  filters <- getFilters object

  case (mWidth, mHeight, mBpc, mDecode, toList filters) of
    ( Just (PDFNumber width)
      , Just (PDFNumber height)
      , Just (PDFNumber 8)
      , Nothing
      , [Filter (PDFName "FlateDecode") parms]
      ) -> do
        compressed <- getStream object
        let bitmapConfig =
              BitmapConfiguration (round width) components BC8Bits
            predictor = predictorOf parms
            expected  = round width * round height * components
        case decodeRawBitmap bitmapConfig predictor compressed of
          Right raw | BS.length raw == expected ->
            return $ Just (PlainBitmap (round width) (round height) raw)
          _anyOtherCase -> return Nothing
    _anyOtherCase -> return Nothing

{-|
Zero out every colour component of pixels whose soft mask sample is exactly
@0@ (fully transparent), leaving other pixels unchanged.

Both bytestrings must describe the same pixel grid: @image@ has
@components@ bytes per pixel, @mask@ has one byte per pixel.
-}
zeroFillPixels :: Int -> ByteString -> ByteString -> ByteString
zeroFillPixels components image mask =
  BSI.unsafeCreate (BS.length image) (`writePixel` 0)
 where
  pixelCount :: Int
  pixelCount = BS.length mask

  writePixel :: Ptr Word8 -> Int -> IO ()
  writePixel dst pixel
    | pixel >= pixelCount = return ()
    | otherwise = do
        writeComponent dst pixel 0
        writePixel dst (pixel + 1)

  writeComponent :: Ptr Word8 -> Int -> Int -> IO ()
  writeComponent dst pixel component
    | component >= components = return ()
    | otherwise = do
        let offset = pixel * components + component
            value
              | BS.index mask pixel == 0 = 0
              | otherwise                = BS.index image offset
        pokeByteOff dst offset value
        writeComponent dst pixel (component + 1)

{-|
Attempt the zero-fill transformation on an Image XObject with a plain
@/SMask@, replacing its stream and filters only when the recompressed result
is strictly smaller than the original stream. Otherwise, the object is
returned unchanged.
-}
zeroFillMaskedImages :: Logging m => PDFObject -> PDFWork m PDFObject
zeroFillMaskedImages object@PDFIndirectObjectWithStream{} = do
  mSubtype    <- getValue "Subtype" object
  mColorSpace <- getValue "ColorSpace" object
  mImageMask  <- getValue "ImageMask" object
  mMaskEntry  <- getValue "Mask" object
  mSMask      <- getValue "SMask" object

  case (mSubtype, mImageMask, mMaskEntry, mSMask, mColorSpace >>= componentsOf) of
    ( Just (PDFName "Image")
      , Nothing
      , Nothing
      , Just smaskRef@(PDFReference _ _)
      , Just components
      ) -> do
        maskObject <- getReference smaskRef
        withMask components object maskObject
    _anyOtherCase -> return object
zeroFillMaskedImages object = return object

{-|
Validate that the referenced mask is itself a plain, unmasked image before
decoding both bitmaps and attempting the zero-fill.
-}
withMask
  :: Logging m => Int -> PDFObject -> PDFObject -> PDFWork m PDFObject
withMask components object maskObject = do
  mMaskSubtype <- getValue "Subtype" maskObject
  mMaskSMask   <- getValue "SMask" maskObject
  mMaskMask    <- getValue "Mask" maskObject

  case (mMaskSubtype, mMaskSMask, mMaskMask) of
    (Just (PDFName "Image"), Nothing, Nothing) -> do
      mImage <- plainBitmap components object
      mMask  <- plainBitmap 1 maskObject
      case (mImage, mMask) of
        (Just image, Just mask)
          | pbWidth image == pbWidth mask && pbHeight image == pbHeight mask
          , BS.elem 0 (pbRaw mask)
          -> applyZeroFill components object image mask
        _anyOtherCase -> return object
    _anyOtherCase -> return object

{-|
Recompress the zero-filled pixels and keep the change only if it is smaller
than the original stream.
-}
applyZeroFill
  :: Logging m
  => Int
  -> PDFObject
  -> PlainBitmap
  -> PlainBitmap
  -> PDFWork m PDFObject
applyZeroFill components object image mask = do
  original <- getStream object
  let filled = zeroFillPixels components (pbRaw image) (pbRaw mask)
  case Flate.compress filled of
    Right recompressed | BS.length recompressed < BS.length original -> do
      sayComparisonP "Zero-fill masked image"
                     (BS.length original)
                     (BS.length recompressed)
      setStream recompressed object
        >>= setFilters (mkFilterList [Filter (PDFName "FlateDecode") PDFNull])
    _anyOtherCase -> return object
