-- |
-- Module      : CCITTG4
-- Description : Minimal CCITT T.6 (Group 4) encoder/decoder for PDF masks.
--
-- Scope deliberately kept small:
--
--   * pure T.6 / Group 4 two-dimensional coding;
--   * packed 1-bit rows, MSB first;
--   * input/output convention: 1 = black, 0 = white;
--   * no EOL, no byte alignment between rows, no EOFB;
--   * the decoder stops after exactly @height@ rows;
--   * unused low bits in the last byte of each decoded row are zero.
--
-- Suitable PDF DecodeParms for bytes produced by 'encodeG4':
--
--   << /K -1
--      /Columns <width>
--      /Rows <height>
--      /EndOfLine false
--      /EncodedByteAlign false
--      /EndOfBlock false
--      /BlackIs1 true
--   >>
--
-- If your PDF image convention is 0 = black, either invert the bitmap before
-- calling encodeG4 or set /BlackIs1 appropriately for the surrounding image.
--
-- The implementation favors clarity and strictness.  It uses lists of change
-- positions per scanline; that is already efficient for the sparse/solid masks
-- for which Group 4 is attractive.
module Codec.Compression.CCITTG4
  ( G4Error(..)
  , encodeG4
  , decodeG4
  ) where


import Data.Bits (Bits (setBit, shiftR, testBit, (.&.)))
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.Kind (Type)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)

type G4Error :: Type
data G4Error
  = InvalidDimensions
  | InvalidBitmapLength !Int !Int -- expected, actual
  | UnexpectedEndOfInput
  | InvalidModeCode
  | InvalidRunCode
  | InvalidChangingElement !Int
  | RowOverflow !Int
  | RowDidNotTerminate
  deriving stock (Eq, Show)

-- | Encode a packed 1-bpp bitmap using pure CCITT T.6 / Group 4.
--
-- Rows are byte-aligned in the *uncompressed* bitmap.  Within each byte the
-- leftmost pixel is the MSB.  A set bit denotes black.
encodeG4 :: Int -> Int -> BS.ByteString -> Either G4Error BS.ByteString
encodeG4 width height src
  | width <= 0 || height < 0
  = Left InvalidDimensions

  | BS.length src /= expected
  = Left (InvalidBitmapLength expected (BS.length src))

  | otherwise
  = Right . finishWriter
          $ foldl encodeOne emptyWriter (zip rows (whiteLine : rows))
  where
    stride :: Int
    stride = (width + 7) `div` 8

    expected :: Int
    expected = stride * height

    rows :: [Changes]
    rows = [ changesOfRow width (BS.take stride (BS.drop (y*stride) src))
           | y <- [0 .. height-1]
           ]

    whiteLine :: Changes
    whiteLine = []  -- no changes: white from x=0 through width

    encodeOne :: BitWriter -> (Changes, Changes) -> BitWriter
    encodeOne bw (cur, ref) = encode2DLine width cur ref bw

-- | Decode exactly @height@ Group-4 scanlines into a packed 1-bpp bitmap.
--
-- The decoder intentionally accepts only the subset emitted by 'encodeG4':
-- pure 2-D T.6 data, with no EOL/EOFB and no per-row byte alignment.
decodeG4 :: Int -> Int -> BS.ByteString -> Either G4Error BS.ByteString
decodeG4 width height src
  | width <= 0 || height < 0
  = Left InvalidDimensions

  | otherwise
  = do
      let
        br0 :: BitReader
        br0 = BitReader src 0

      (rows, _) <- decodeRows height [] br0 []
      pure (BS.concat (map (packRow width) rows))
  where
    decodeRows
      :: Int
      -> Changes
      -> BitReader
      -> [Changes]
      -> Either G4Error ([Changes], BitReader)
    decodeRows 0 _ br acc = Right (reverse acc, br)

    decodeRows n ref br acc = do
      (cur, br') <- decode2DLine width ref br
      decodeRows (n-1) cur br' (cur:acc)

--------------------------------------------------------------------------------
-- Scanline representation
--
-- A row is represented by positions where the colour changes.  The colour
-- immediately before x=0 is white.  Thus [0,10] means black pixels 0..9.

type Changes :: Type
type Changes = [Int]

changesOfRow :: Int -> BS.ByteString -> Changes
changesOfRow width bs = go 0 False []
  where
    pixel :: Int -> Bool
    pixel x =
      let
        w :: Word8
        w = BS.index bs (x `div` 8)

        m :: Word8
        m = 0x80 `shiftR` (x .&. 7)
      in
        w .&. m /= 0

    go :: Int -> Bool -> Changes -> Changes
    go x !old acc
      | x >= width = reverse acc
      | otherwise
      = let
          p :: Bool
          !p = pixel x
        in
          if p /= old
            then go (x + 1) p (x:acc)
            else go (x + 1) old acc

packRow :: Int -> Changes -> BS.ByteString
packRow width cs = BS.pack [mkByte b | b <- [0 .. stride-1]]
  where
    stride :: Int
    stride = (width + 7) `div` 8

    blackAt :: Int -> Bool
    blackAt x
      | x >= width = False
      | otherwise  = odd (length (takeWhile (<= x) cs))

    mkByte :: Int -> Word8
    mkByte b =
      foldl (\w k ->
               let
                x :: Int
                x = b * 8 + k
               in
                if blackAt x then setBit w (7-k) else w
            )
            0 [0..7]

-- First relevant changing element on the reference line. At the start of a
-- row, a0 is the imaginary white element before x=0, so x=0 is eligible;
-- after the first coding operation T.6 requires b1 to be strictly right of a0.
b1b2 :: Int -> Changes -> Int -> Bool -> Bool -> (Int, Int)
b1b2 width ref a0 colour atStart =
  case filter toRight candidates of
    []     -> (width, width)
    (b1:_) -> (b1, nextChangeAfter width ref b1)
  where
    -- change number 0 changes white->black, number 1 black->white, ...
    toRight p = if atStart then p >= a0 else p > a0
    candidates =
      [ p
      | (i,p) <- zip [0 :: Int ..] ref
      , let
          after :: Bool
          after = even i -- True = black after even-numbered change
      , after /= colour
      ]

nextChangeAfter :: Int -> Changes -> Int -> Int
nextChangeAfter width cs x = fromMaybe width (find (> x) cs)

nextChangeForColour :: Int -> Changes -> Int -> Bool -> Int
nextChangeForColour width cs a0 colour =
  case [ p
       | (i,p) <- zip [0 :: Int ..] cs
       , p >= a0
       , let
           after :: Bool
           after = even i
       , after /= colour
       ] of
    (p:_) -> p
    []    -> width

--------------------------------------------------------------------------------
-- T.6 2-D modes

type Mode :: Type
data Mode
  = Pass
  | Horiz
  | Vert !Int
  deriving stock (Eq, Show)

modeBits :: Mode -> String
modeBits Pass        = "0001"
modeBits Horiz       = "001"
modeBits (Vert 0)    = "1"
modeBits (Vert 1)    = "011"
modeBits (Vert (-1)) = "010"
modeBits (Vert 2)    = "000011"
modeBits (Vert (-2)) = "000010"
modeBits (Vert 3)    = "0000011"
modeBits (Vert (-3)) = "0000010"
modeBits _           = error "modeBits: vertical displacement outside -3..3"

encode2DLine :: Int -> Changes -> Changes -> BitWriter -> BitWriter
encode2DLine width cur ref = go True 0 False
  where
    go :: Bool -> Int -> Bool -> BitWriter -> BitWriter
    go !atStart !a0 !colour bw
      | a0 >= width = bw
      | otherwise =
          let
            a1 :: Int
            a1 = nextChangeForColour width cur a0 colour

            a2 :: Int
            a2 = nextChangeAfter width cur a1

            b1 :: Int
            b2 :: Int
            (b1,b2) = b1b2 width ref a0 colour atStart

            d :: Int
            d = a1 - b1
          in
            if b2 < a1
              then
                go False b2 colour (putCode (modeBits Pass) bw)
              else
                if d >= (-3) && d <= 3
                  then
                    go False a1 (not colour) (putCode (modeBits (Vert d)) bw)
                  else
                    let
                      bw1 :: BitWriter
                      bw1 = putCode (modeBits Horiz) bw

                      bw2 :: BitWriter
                      bw2 = putRun colour (a1-a0) bw1

                      bw3 :: BitWriter
                      bw3 = putRun (not colour) (a2-a1) bw2
                    in
                      go False a2 colour bw3

decode2DLine
  :: Int
  -> Changes
  -> BitReader
  -> Either G4Error (Changes, BitReader)
decode2DLine width ref = go True 0 False []
  where
    go !atStart !a0 !colour acc br
      | a0 == width
      = Right (reverse acc, br)

      | a0 > width
      = Left (RowOverflow a0)

      | otherwise
      = do
          (m, br1) <- getMode br
          let
            b1 :: Int
            b2 :: Int
            (b1,b2) = b1b2 width ref a0 colour atStart

          case m of
            Pass ->
              if b2 <= a0
                then Left RowDidNotTerminate
                else go False b2 colour acc br1

            Vert d -> do
              let
                a1 :: Int
                a1 = b1 + d

              if a1 < a0 || a1 > width
                then
                  Left (InvalidChangingElement a1)
                else
                  let
                    acc' :: Changes
                    acc' = if a1 < width then a1:acc else acc
                  in
                    go False a1 (not colour) acc' br1

            Horiz -> do
              (r1, br2) <- getRun colour br1
              (r2, br3) <- getRun (not colour) br2

              let
                a1 :: Int
                a1 = a0 + r1

                a2 :: Int
                a2 = a1 + r2

              if a1 < a0 || a1 > width
                then
                  Left (RowOverflow a1)
                else
                  if a2 < a1 || a2 > width
                    then Left (RowOverflow a2)
                    else
                      let
                        acc1 :: Changes
                        acc1 = if a1 < width then a1:acc else acc

                        acc2 :: Changes
                        acc2 = if a2 < width then a2:acc1 else acc1
                      in
                        if a2 == a0
                          then Left RowDidNotTerminate
                          else go False a2 colour acc2 br3

--------------------------------------------------------------------------------
-- Modified Huffman run codes used by horizontal mode

-- (run length, code)
whiteTerm, blackTerm :: [(Int, String)]
whiteTerm = zip [0..63]
  [ "00110101"
  , "000111"
  , "0111"
  , "1000"
  , "1011"
  , "1100"
  , "1110"
  , "1111"
  , "10011"
  , "10100"
  , "00111"
  , "01000"
  , "001000"
  , "000011"
  , "110100"
  , "110101"
  , "101010"
  , "101011"
  , "0100111"
  , "0001100"
  , "0001000"
  , "0010111"
  , "0000011"
  , "0000100"
  , "0101000"
  , "0101011"
  , "0010011"
  , "0100100"
  , "0011000"
  , "00000010"
  , "00000011"
  , "00011010"
  , "00011011"
  , "00010010"
  , "00010011"
  , "00010100"
  , "00010101"
  , "00010110"
  , "00010111"
  , "00101000"
  , "00101001"
  , "00101010"
  , "00101011"
  , "00101100"
  , "00101101"
  , "00000100"
  , "00000101"
  , "00001010"
  , "00001011"
  , "01010010"
  , "01010011"
  , "01010100"
  , "01010101"
  , "00100100"
  , "00100101"
  , "01011000"
  , "01011001"
  , "01011010"
  , "01011011"
  , "01001010"
  , "01001011"
  , "00110010"
  , "00110011"
  , "00110100"
  ]

blackTerm = zip [0..63]
  [ "0000110111"
  , "010"
  , "11"
  , "10"
  , "011"
  , "0011"
  , "0010"
  , "00011"
  , "000101"
  , "000100"
  , "0000100"
  , "0000101"
  , "0000111"
  , "00000100"
  , "00000111"
  , "000011000"
  , "0000010111"
  , "0000011000"
  , "0000001000"
  , "00001100111"
  , "00001101000"
  , "00001101100"
  , "00000110111"
  , "00000101000"
  , "00000010111"
  , "00000011000"
  , "000011001010"
  , "000011001011"
  , "000011001100"
  , "000011001101"
  , "000001101000"
  , "000001101001"
  , "000001101010"
  , "000001101011"
  , "000011010010"
  , "000011010011"
  , "000011010100"
  , "000011010101"
  , "000011010110"
  , "000011010111"
  , "000001101100"
  , "000001101101"
  , "000011011010"
  , "000011011011"
  , "000001010100"
  , "000001010101"
  , "000001010110"
  , "000001010111"
  , "000001100100"
  , "000001100101"
  , "000001010010"
  , "000001010011"
  , "000000100100"
  , "000000110111"
  , "000000111000"
  , "000000100111"
  , "000000101000"
  , "000001011000"
  , "000001011001"
  , "000000101011"
  , "000000101100"
  , "000001011010"
  , "000001100110"
  , "000001100111"
  ]

whiteMakeup, blackMakeup, commonMakeup :: [(Int, String)]
whiteMakeup =
  [ ( 64, "11011" )
  , ( 128, "10010" )
  , ( 192, "010111" )
  , ( 256, "0110111" )
  , ( 320, "00110110" )
  , ( 384, "00110111" )
  , ( 448, "01100100" )
  , ( 512, "01100101" )
  , ( 576, "01101000" )
  , ( 640, "01100111" )
  , ( 704, "011001100" )
  , ( 768, "011001101" )
  , ( 832, "011010010" )
  , ( 896, "011010011" )
  , ( 960, "011010100" )
  , ( 1024, "011010101" )
  , ( 1088, "011010110" )
  , ( 1152, "011010111" )
  , ( 1216, "011011000" )
  , ( 1280, "011011001" )
  , ( 1344, "011011010" )
  , ( 1408, "011011011" )
  , ( 1472, "010011000" )
  , ( 1536, "010011001" )
  , ( 1600, "010011010" )
  , ( 1664, "011000" )
  , ( 1728, "010011011" )
  ]

blackMakeup =
  [ ( 64, "0000001111" )
  , ( 128, "000011001000" )
  , ( 192, "000011001001" )
  , ( 256, "000001011011" )
  , ( 320, "000000110011" )
  , ( 384, "000000110100" )
  , ( 448, "000000110101" )
  , ( 512, "0000001101100" )
  , ( 576, "0000001101101" )
  , ( 640, "0000001001010" )
  , ( 704, "0000001001011" )
  , ( 768, "0000001001100" )
  , ( 832, "0000001001101" )
  , ( 896, "0000001110010" )
  , ( 960, "0000001110011" )
  , ( 1024, "0000001110100" )
  , ( 1088, "0000001110101" )
  , ( 1152, "0000001110110" )
  , ( 1216, "0000001110111" )
  , ( 1280, "0000001010010" )
  , ( 1344, "0000001010011" )
  , ( 1408, "0000001010100" )
  , ( 1472, "0000001010101" )
  , ( 1536, "0000001011010" )
  , ( 1600, "0000001011011" )
  , ( 1664, "0000001100100" )
  , ( 1728, "0000001100101" )
  ]

commonMakeup =
  [ ( 1792, "00000001000" )
  , ( 1856, "00000001100" )
  , ( 1920, "00000001101" )
  , ( 1984, "000000010010" )
  , ( 2048, "000000010011" )
  , ( 2112, "000000010100" )
  , ( 2176, "000000010101" )
  , ( 2240, "000000010110" )
  , ( 2304, "000000010111" )
  , ( 2368, "000000011100" )
  , ( 2432, "000000011101" )
  , ( 2496, "000000011110" )
  , ( 2560, "000000011111" )
  ]

putRun :: Bool -> Int -> BitWriter -> BitWriter
putRun black n0 bw0
  | n0 < 0
  = error "putRun: negative run"

  | otherwise
  = let
      bw1 :: BitWriter
      remn :: Int
      (bw1, remn) = putLarge n0 bw0

      makeup :: [(Int, String)]
      makeup = if black then blackMakeup else whiteMakeup

      term :: [(Int, String)]
      term = if black then blackTerm else whiteTerm

      q :: Int
      r :: Int
      (q,r) = remn `divMod` 64

      bw2 :: BitWriter
      bw2 = if q == 0
              then bw1
              else putCode (lookupCode (q*64) makeup) bw1
    in
      putCode (lookupCode r term) bw2
  where
    putLarge n bw
      | n >= 2560
      = putLarge (n-2560) (putCode (lookupCode 2560 commonMakeup) bw)

      | n >= 1792
      = let
          k :: Int
          k = (n `div` 64) * 64

          k' :: Int
          k' = min 2560 k
        in
          (putCode (lookupCode k' commonMakeup) bw, n-k')

      | otherwise
      = (bw, n)

lookupCode :: Int -> [(Int, String)] -> String
lookupCode n tab =
  case lookup n tab of
    Just s  -> s
    Nothing -> error ("missing CCITT code for run " ++ show n)

getRun :: Bool -> BitReader -> Either G4Error (Int, BitReader)
getRun black = go 0
  where
    table :: [(Int, String)]
    table = (if black then blackTerm ++ blackMakeup
                      else whiteTerm ++ whiteMakeup) ++ commonMakeup

    go :: Int -> BitReader -> Either G4Error (Int, BitReader)
    go !acc br = do
      ((n,isTerm), br') <- getRunSymbol table br
      let
        acc' :: Int
        acc' = acc + n

      if isTerm
        then Right (acc',br')
        else go acc' br'

getRunSymbol
  :: [(Int, String)]
  -> BitReader
  -> Either G4Error ((Int,Bool), BitReader)
getRunSymbol tab = walk ""
  where
    walk :: String -> BitReader -> Either G4Error ((Int,Bool), BitReader)
    walk pref br
      | length pref > 13
      = Left InvalidRunCode

      | otherwise
      = do
          (b,br') <- getBit br

          let
            p :: String
            p = pref ++ [if b then '1' else '0']

            exact :: [(Int, String)]
            exact = [ (n,s) | (n,s) <- tab, s == p ]

            prefixExists :: Bool
            prefixExists = any ((p `isPrefixOf`) . snd) tab

          case exact of
            ((n,_):_) -> Right ((n, n < 64), br')
            [] | prefixExists -> walk p br'
               | otherwise    -> Left InvalidRunCode

--------------------------------------------------------------------------------
-- Mode decoder

getMode :: BitReader -> Either G4Error (Mode, BitReader)
getMode = walk ""
  where
    modes :: [(String, Mode)]
    modes =
      [ ("1",       Vert 0)
      , ("011",     Vert 1)
      , ("010",     Vert (-1))
      , ("001",     Horiz)
      , ("0001",    Pass)
      , ("000011",  Vert 2)
      , ("000010",  Vert (-2))
      , ("0000011", Vert 3)
      , ("0000010", Vert (-3))
      ]

    walk :: String -> BitReader -> Either G4Error (Mode, BitReader)
    walk pref br
      | length pref >= 7 = Left InvalidModeCode
      | otherwise = do
          (b,br') <- getBit br

          let
            p :: String
            p = pref ++ [if b then '1' else '0']

          case lookup p modes of
            Just m  -> Right (m,br')
            Nothing | any ((p `isPrefixOf`) . fst) modes -> walk p br'
                    | otherwise -> Left InvalidModeCode

--------------------------------------------------------------------------------
-- Bit I/O, MSB first

type BitWriter :: Type
data BitWriter = BitWriter
  { _bwBytes :: !BB.Builder
  , _bwCur   :: !Word8
  , _bwUsed  :: !Int
  }

emptyWriter :: BitWriter
emptyWriter = BitWriter mempty 0 0

putCode :: String -> BitWriter -> BitWriter
putCode s bw = foldl (flip putBit) bw (map (== '1') s)

putBit :: Bool -> BitWriter -> BitWriter
putBit bit' (BitWriter out cur used) =
  let
    cur' :: Word8
    cur'  = if bit' then setBit cur (7-used) else cur

    used' :: Int
    used' = used + 1
  in
    if used' == 8
      then BitWriter (out <> BB.word8 cur') 0 0
      else BitWriter out cur' used'

finishWriter :: BitWriter -> BS.ByteString
finishWriter (BitWriter out cur used) =
  BL.toStrict . BB.toLazyByteString
              $ if used == 0
                  then out
                  else out <> BB.word8 cur

type BitReader :: Type
data BitReader = BitReader
  { _brInput :: !BS.ByteString
  , _brPos   :: !Int
  }

getBit :: BitReader -> Either G4Error (Bool, BitReader)
getBit (BitReader bs p)
  | p >= BS.length bs * 8 = Left UnexpectedEndOfInput
  | otherwise =
      let
        w :: Word8
        w = BS.index bs (p `div` 8)

        b :: Bool
        b = testBit w (7 - (p .&. 7))
      in
        Right (b, BitReader bs (p + 1))

--------------------------------------------------------------------------------
-- Tiny local helper (keeps dependencies minimal)

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _          = True
isPrefixOf _  []         = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
