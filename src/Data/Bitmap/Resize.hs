-- | Area-average downsampling of interleaved 8/16-bit bitmap samples.
module Data.Bitmap.Resize (resizeBitmap) where

import Control.Monad (guard)

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Vector qualified as V
import Data.Word (Word8)

{- | Downsample without changing the number of channels or the sample depth.
16-bit samples use PDF's big-endian byte order. Integer overlap weights account
for every source pixel, including fractional edges, avoiding aliasing from
nearest-neighbor sampling. Invalid geometry, upsampling and buffer sizes fail.
-}
resizeBitmap
  :: Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> ByteString
  -> Maybe ByteString
resizeBitmap width height components bits targetWidth targetHeight input = do
  guard (width > 0 && height > 0 && components > 0)
  guard (bits == 8 || bits == 16)
  guard (  targetWidth > 0
        && targetWidth <= width
        && targetHeight > 0
        && targetHeight <= height
        )
  guard (toInteger (BS.length input) == toInteger width
                                     *  toInteger height
                                     *  toInteger components
                                     *  toInteger bytesPerSample
        )

  if (width, height) == (targetWidth, targetHeight)
    then
      Just input
    else
      Just $ fst
           $ BS.unfoldrN ( targetWidth
                         * targetHeight
                         * components
                         * bytesPerSample
                         )
                         next
                         (0, Nothing)
 where
  bytesPerSample :: Int
  bytesPerSample = bits `div` 8

  xWeights :: V.Vector [(Int, Integer)]
  xWeights = V.fromList (axisWeights width targetWidth)

  yWeights :: V.Vector [(Int, Integer)]
  yWeights = V.fromList (axisWeights height targetHeight)

  -- Fill the output buffer directly, without constructing a list of all bytes.
  -- Keep the low byte of a 16-bit sample so it need not be averaged twice.
  next :: (Int, Maybe Word8) -> Maybe (Word8, (Int, Maybe Word8))
  next (index, Just low) = Just (low, (index, Nothing))
  next (index, Nothing) =
    let
      pixel :: Int
      channel :: Int
      (pixel, channel) = index `divMod` components

      x :: Int
      y :: Int
      (y, x) = pixel `divMod` targetWidth

      value :: Integer
      value = average (xWeights V.! x) (yWeights V.! y) channel
    in
      if bits == 8
        then Just ( fromIntegral value
                  , (index + 1, Nothing)
                  )
        else Just ( fromIntegral (value `div` 256)
                  , (index + 1, Just (fromIntegral (value `mod` 256)))
                  )

  sample :: Int -> Int -> Int -> Integer
  sample x y channel =
    let
      index :: Int
      index = ((y * width + x) * components + channel) * bytesPerSample

      high :: Integer
      high = fromIntegral (BS.index input index)
    in
      if bits == 8
        then high
        else high * 256 + fromIntegral (BS.index input (index + 1))

  average :: [(Int, Integer)] -> [(Int, Integer)] -> Int -> Integer
  average xs ys channel =
    let
      total :: Integer
      total = sum [ sample x y channel * wx * wy
                  | (x, wx) <- xs
                  , (y, wy) <- ys
                  ]

      area :: Integer
      area = toInteger width * toInteger height
    in
      (total + area `div` 2) `div` area

-- | Overlap lengths in a common integer coordinate system. Each target
-- interval has total weight equal to the source length.
axisWeights :: Int -> Int -> [[(Int, Integer)]]
axisWeights source target =
  [ let
      start :: Integer
      start = toInteger i * toInteger source

      end :: Integer
      end = toInteger (i + 1) * toInteger source

      scale :: Integer
      scale = toInteger target
    in
      [(fromInteger j, min end ((j + 1) * scale) - max start (j * scale))
      | j <- [start `div` scale .. (end - 1) `div` scale]
      ]
  | i <- [0 .. target - 1]
  ]
