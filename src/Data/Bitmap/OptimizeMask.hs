-- | Conservative alpha candidates. All lossy candidates stay within three
-- alpha units of the input; histogram classification alone never permits
-- deleting a small but high-contrast feature.
module Data.Bitmap.OptimizeMask (maskCandidates, packMask) where

import Data.Bitmap.MaskAnalysis
  (MaskAnalysis (largeDifferenceFraction, maskClass), MaskClass (SmoothMask))
import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.List (nub)
import Data.Word (Word8)

import Foreign (castPtr)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peek, poke)

-- | Original first, then snap/quantization and optional 2x/4x reduction.
-- Call only with validated dimensions and one alpha byte per pixel.
maskCandidates
  :: Bool
  -> MaskAnalysis
  -> Int
  -> Int
  -> ByteString
  -> [(Bool, Int, Int, ByteString)]
maskCandidates False _analysis width height alpha =
  [(False, width, height, alpha)]

maskCandidates True analysis width height alpha =
  nub
    ( [ (True, width, height, candidate)
      | candidate <- quantized
      , candidate /= alpha
      , close alpha candidate
      ]
    ++ reduced
    )
 where
  -- Snap near-transparent and near-opaque samples to exact endpoints.
  snap :: Word8 -> Word8
  snap value | value <= 3   = 0
             | value >= 252 = 255
             | otherwise    = value

  -- Map an alpha byte to the nearest value on an evenly spaced palette.
  quantize :: Int -> Word8 -> Word8
  quantize levels value = fromIntegral paletteValue
   where
    steps :: Int
    steps = levels - 1

    paletteIndex :: Int
    paletteIndex = (fromIntegral (snap value) * steps + 127) `quot` 255

    paletteValue :: Int
    paletteValue = (paletteIndex * 255 + steps `quot` 2) `quot` steps

  -- Snapped and palette-quantized versions of the source alpha plane.
  quantized :: [ByteString]
  quantized = BS.map snap alpha : [BS.map (quantize n) alpha | n <- [64,32,16]]

  -- Downsample only smooth masks with no exact endpoints or large changes.
  reduced :: [(Bool, Int, Int, ByteString)]
  reduced
    | maskClass analysis /= SmoothMask || largeDifferenceFraction analysis /= 0
      -- Preserve exact transparent/opaque pixels and avoid exposing color
      -- pixels previously zero-filled under transparent mask samples.
      || BS.any (\v -> v == 0 || v == 255) alpha
    = []

    | otherwise
    = [ (True, w, h, small)
      | factor <- [2, 4]
      , width `mod` factor == 0, height `mod` factor == 0
      , let w = width `div` factor
      , let h = height `div` factor
      , w > 0
      , h > 0
      , let small = BS.pack
              [ fromIntegral
                ( ( sum
                      [ fromIntegral (BS.index alpha ((y * factor + dy) * width
                                                      + x * factor + dx)) :: Int
                      | dy <- [0 .. factor - 1]
                      , dx <- [0 .. factor - 1]
                      ]
                  + factor
                  * factor `div` 2
                  )
                  `div` (factor * factor)
                )
              | y <- [0 .. h - 1]
              , x <- [0 .. w - 1]
              ]
      -- Bound both nearest-neighbor and bilinear reconstruction: every
      -- neighboring reduced sample must be close to the original sample.
      , and [ abs (fromIntegral (BS.index alpha (y * width + x))
                   - (fromIntegral (BS.index small (sy * w + sx)) :: Int)) <= 3
            | y <- [0 .. height - 1]
            , x <- [0 .. width - 1]
            , let x0 = (2 * x + 1 - factor) `div` (2 * factor)
            , let y0 = (2 * y + 1 - factor) `div` (2 * factor)
            , sx <- nub [ max 0 (min (w - 1) x0)
                        , max 0 (min (w - 1) (x0 + 1))
                        ]
            , sy <- nub [ max 0 (min (h - 1) y0)
                        , max 0 (min (h - 1) (y0 + 1))
                        ]
            ]
      ]

-- | Check that corresponding alpha samples differ by at most three units.
close :: ByteString -> ByteString -> Bool
close original candidate = and (BS.zipWith closeEnough original candidate)
 where
  closeEnough :: Word8 -> Word8 -> Bool
  closeEnough a b = abs (fromIntegral a - (fromIntegral b :: Int)) <= 3

-- | Pack binary alpha, MSB first, restarting each row and clearing padding.
packMask :: Int -> Int -> ByteString -> ByteString
packMask width height alpha
  | width <= 0 || height <= 0
  = BS.empty

  | otherwise
  = BSI.unsafeCreate outLen $ \dst ->
      BS.useAsCString alpha $ \src0 ->
        goRows (castPtr src0) dst 0
 where
  bytesPerRow :: Int
  bytesPerRow = (width + 7) `quot` 8

  outLen :: Int
  outLen = bytesPerRow * height

  goRows :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
  goRows src dst !y
    | y >= height = pure ()
    | otherwise   = goCols src dst y 0 >> goRows src dst (y + 1)

  goCols :: Ptr Word8 -> Ptr Word8 -> Int -> Int -> IO ()
  goCols src dst !y !byteX
    | byteX >= bytesPerRow
    = pure ()

    | otherwise
    = do
        let
          x :: Int
          !x = byteX * 8

          srcOff :: Int
          !srcOff = y * width + x

          dstOff :: Int
          !dstOff = y * bytesPerRow + byteX

        !b <- packByte (src `plusPtr` srcOff) (min 8 (width - x))
        poke (dst `plusPtr` dstOff) b

        goCols src dst y (byteX + 1)

  packByte :: Ptr Word8 -> Int -> IO Word8
  packByte src !n = loop 0 0
   where
    loop :: Int -> Word8 -> IO Word8
    loop !bit !acc
      | bit >= n
      = pure acc

      | otherwise
      = do
          a <- peek (src `plusPtr` bit) :: IO Word8

          let
            acc' :: Word8
            !acc' = if a == 255 then acc .|. (1 `shiftL` (7 - bit))
                                else acc

          loop (bit + 1) acc'
