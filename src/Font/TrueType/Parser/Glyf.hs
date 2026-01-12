{-|
Parse the TrueType "glyf" (glyph data) table.

The glyf table contains the actual outline data for each glyph. This module
provides parsers to decode glyph descriptions including both simple and
composite glyphs.

== Parsing Strategy

The glyf table cannot be parsed sequentially without the location table (loca),
which provides offsets and lengths for each glyph. The recommended approach:

1. Parse the loca table to get glyph offsets
2. For each glyph, extract the byte range from glyf table data
3. Use 'singleGlyphP' to parse the individual glyph

== Glyph Types

Glyphs are identified by their header's numberOfContours field:

* 0: Empty glyph (no outline)
* Positive: Simple glyph with that many contours
* Negative: Composite glyph built from components

== Coordinate Encoding

Simple glyphs use delta encoding with variable-length coordinates based on
flags. The parsers handle expansion of repeated flags and reconstruction of
absolute coordinates from deltas.
-}
module Font.TrueType.Parser.Glyf
  ( glyphHeaderP
  , simpleGlyphP
  , compositeGlyphP
  , singleGlyphP
  , glyphP
  ) where

import Control.Monad (replicateM)

import Data.Binary.Get (getInt16be, getWord16be, getWord8)
import Data.Binary.Parser (Get, getByteString, isEmpty, label)
import Data.Bits (testBit, (.&.))
import Data.Int (Int16)
import Data.Word (Word8, Word16)

import Font.TrueType.FontTable.GlyphTable
  ( CompositeGlyphData (CompositeGlyphData, cgArgument1, cgArgument2, cgFlags, cgGlyphIndex, cgInstructions, cgTransformation)
  , Glyph (CompositeGlyph, EmptyGlyph, SimpleGlyph)
  , GlyphTable (GlyphTable)
  , GlyphHeader (GlyphHeader, ghNumberOfContours, ghXMax, ghXMin, ghYMax, ghYMin)
  , SimpleGlyphData (SimpleGlyphData, sgEndPtsOfContours, sgFlags, sgInstructionLength, sgInstructions, sgXCoordinates, sgYCoordinates)
  , TransformationMatrix (NoScale, Transform2x2, UniformScale, XYScale)
  )

-- | Bit 0: Arguments are 16-bit words (vs 8-bit bytes)
argWordsFlag :: Word16
argWordsFlag = 0x0001

-- | Bit 3: Component has a uniform scale factor
weHaveAScaleFlag :: Word16
weHaveAScaleFlag = 0x0008

-- | Bit 5: At least one more component follows this one
moreComponentsFlag :: Word16
moreComponentsFlag = 0x0020

-- | Bit 6: Component has separate X and Y scale factors
weHaveAnXAndYScaleFlag :: Word16
weHaveAnXAndYScaleFlag = 0x0040

-- | Bit 7: Component has a 2x2 affine transformation matrix
weHaveATwoByTwoFlag :: Word16
weHaveATwoByTwoFlag = 0x0080

-- | Bit 8: Composite glyph has instructions following all components
weHaveInstructionsFlag :: Word16
weHaveInstructionsFlag = 0x0100

{-|
Parse a glyph header.

Reads the common header present in all glyph descriptions including the number
of contours and bounding box coordinates.
-}
glyphHeaderP :: Get GlyphHeader
glyphHeaderP = label "glyphHeader" $ do
  numberOfContours <- getInt16be
  xMin             <- getInt16be
  yMin             <- getInt16be
  xMax             <- getInt16be
  yMax             <- getInt16be
  return GlyphHeader
    { ghNumberOfContours = numberOfContours
    , ghXMin             = xMin
    , ghYMin             = yMin
    , ghXMax             = xMax
    , ghYMax             = yMax
    }

{-|
Parse flags for a simple glyph, expanding repeat flags.

Reads flag bytes and processes the repeat flag (bit 3) to expand repeated flags
into the full list. When bit 3 is set, the next byte indicates how many times to
repeat the current flag (actual count is byte value + 1).

This compression is used because consecutive points often have the same flags,
especially for straight line segments.

@numPoints@: Total number of points expected

Returns a list of flags, one per point.
-}
parseFlagsP :: Int -> Get [Word8]
parseFlagsP numPoints = label "flags" $ go numPoints []
  where
    go 0 acc = return (reverse acc)
    go n acc = do
      flag <- getWord8
      if testBit flag 3  -- repeat flag
        then do
          repeatCount <- getWord8
          let count = fromIntegral repeatCount + 1
          go (n - count) (replicate count flag ++ acc)
        else go (n - 1) (flag : acc)

{-|
Parse x-coordinates based on flags.

Reads x-coordinates using the flag bits to determine encoding:

* Bit 1 (xShortVector): If set, coordinate is 1 byte (unsigned)
* Bit 4 (xIsSameOrPositive): Interpretation depends on bit 1:
    - If bit 1 set: value is positive (else negative)
    - If bit 1 clear: coordinate is same as previous (else read Int16 delta)

Coordinates are stored as deltas and reconstructed to absolute values. This
delta encoding significantly reduces file size for typical glyphs.

@flags@: List of flags, one per point

Returns absolute x-coordinates for each point.
-}
parseXCoordinatesP :: [Word8] -> Get [Int16]
parseXCoordinatesP flags = label "xCoordinates" $ go flags 0 []
  where
    go :: [Word8] -> Int16 -> [Int16] -> Get [Int16]
    go [] _ acc = return (reverse acc)
    go (flag : fs) prev acc = do
      x <- if testBit flag 1  -- xShortVector
        then do
          byte <- getWord8
          let val = if testBit flag 4  -- xIsSameOrPositive
                then fromIntegral byte
                else negate (fromIntegral byte)
          return (prev + val)
        else if testBit flag 4  -- xIsSameOrPositive (same as previous)
          then return prev
          else do
            delta <- getInt16be
            return (prev + delta)
      go fs x (x : acc)

{-|
Parse y-coordinates based on flags.

Reads y-coordinates using the flag bits to determine encoding:

* Bit 2 (yShortVector): If set, coordinate is 1 byte (unsigned)
* Bit 5 (yIsSameOrPositive): Interpretation depends on bit 2:
    - If bit 2 set: value is positive (else negative)
    - If bit 2 clear: coordinate is same as previous (else read Int16 delta)

Coordinates are stored as deltas and reconstructed to absolute values. This
delta encoding significantly reduces file size for typical glyphs.

@flags@: List of flags, one per point

Returns absolute y-coordinates for each point.
-}
parseYCoordinatesP :: [Word8] -> Get [Int16]
parseYCoordinatesP flags = label "yCoordinates" $ go flags 0 []
  where
    go :: [Word8] -> Int16 -> [Int16] -> Get [Int16]
    go [] _ acc = return (reverse acc)
    go (flag : fs) prev acc = do
      y <- if testBit flag 2  -- yShortVector
        then do
          byte <- getWord8
          let val = if testBit flag 5  -- yIsSameOrPositive
                then fromIntegral byte
                else negate (fromIntegral byte)
          return (prev + val)
        else if testBit flag 5  -- yIsSameOrPositive (same as previous)
          then return prev
          else do
            delta <- getInt16be
            return (prev + delta)
      go fs y (y : acc)

{-|
Parse a simple glyph.

Reads the outline data for a simple glyph including:

1. Contour end points - indices of last point in each contour
2. Instructions - TrueType hinting bytecode
3. Flags - one per point, controlling coordinate encoding
4. X-coordinates - delta-encoded with variable-length representation
5. Y-coordinates - delta-encoded with variable-length representation

The number of points is determined from the maximum end point index. All
coordinate arrays must have exactly this many entries.

@header@: Glyph header containing numberOfContours and bounding box

Returns parsed simple glyph data structure.
-}
simpleGlyphP :: GlyphHeader -> Get SimpleGlyphData
simpleGlyphP header = label "simpleGlyph" $ do
  let numContours = fromIntegral (ghNumberOfContours header)

  -- Read end points of contours
  endPtsOfContours <- replicateM numContours getWord16be

  -- Read instructions
  instructionLength <- getWord16be
  instructions <- getByteString (fromIntegral instructionLength)

  -- Calculate number of points
  let numPoints = if null endPtsOfContours
        then 0
        else fromIntegral (maximum endPtsOfContours) + 1

  -- Read flags
  flags <- parseFlagsP numPoints

  -- Read coordinates
  xCoordinates <- parseXCoordinatesP flags
  yCoordinates <- parseYCoordinatesP flags

  return SimpleGlyphData
    { sgEndPtsOfContours  = endPtsOfContours
    , sgInstructionLength = instructionLength
    , sgInstructions      = instructions
    , sgFlags             = flags
    , sgXCoordinates      = xCoordinates
    , sgYCoordinates      = yCoordinates
    }

{-|
Parse transformation matrix based on flags.

Reads the transformation matrix for a composite glyph component based on the
scale flags present. The transformation type is determined by checking flags in
order of complexity:

1. WE_HAVE_A_TWO_BY_TWO (0x0080): Full 2x2 matrix [a b; c d] (8 bytes)
2. WE_HAVE_AN_X_AND_Y_SCALE (0x0040): Separate X/Y scales (4 bytes)
3. WE_HAVE_A_SCALE (0x0008): Uniform scale (2 bytes)
4. None: No transformation (0 bytes)

@flags@: Component flags indicating which transformation is present

Returns the appropriate transformation matrix variant.
-}
parseTransformationP :: Word16 -> Get TransformationMatrix
parseTransformationP flags
  | flags .&. weHaveATwoByTwoFlag /= 0 = label "transform2x2" $ do
      a <- getInt16be
      b <- getInt16be
      c <- getInt16be
      Transform2x2 a b c <$> getInt16be
  | flags .&. weHaveAnXAndYScaleFlag /= 0 = label "xyScale" $ do
      xScale <- getInt16be
      XYScale xScale <$> getInt16be
  | flags .&. weHaveAScaleFlag /= 0 = label "uniformScale" $ do
      UniformScale <$> getInt16be
  | otherwise = return NoScale

{-|
Parse a single composite glyph component.

Reads one component of a composite glyph:

1. Flags (Word16) - control interpretation of remaining fields
2. Glyph index (Word16) - which glyph to use as component
3. Arguments - offsets or point indices (bytes or words based on
   ARG_1_AND_2_ARE_WORDS flag)
4. Transformation matrix - scaling/transformation (optional, based on flags)

The arguments (arg1, arg2) are interpreted based on flags:

* If ARGS_ARE_XY_VALUES: X and Y offsets for positioning component
* Otherwise: Point numbers for matching (arg1 in parent, arg2 in component)

Instructions are not read here but after all components in 'compositeGlyphP'.

Returns a composite component with empty instructions field.
-}
parseCompositeComponentP :: Get CompositeGlyphData
parseCompositeComponentP = label "compositeComponent" $ do
  flags <- getWord16be
  glyphIndex <- getWord16be

  -- Read arguments (either words or bytes)
  (arg1, arg2) <- if flags .&. argWordsFlag /= 0
    then do
      a1 <- getInt16be
      a2 <- getInt16be
      return (a1, a2)
    else do
      a1 <- getWord8
      a2 <- getWord8
      return (fromIntegral a1, fromIntegral a2)

  -- Read transformation matrix
  transformation <- parseTransformationP flags

  return CompositeGlyphData
    { cgFlags          = flags
    , cgGlyphIndex     = glyphIndex
    , cgArgument1      = arg1
    , cgArgument2      = arg2
    , cgTransformation = transformation
    , cgInstructions   = ""  -- Instructions are read separately after all components
    }

{-|
Parse all components of a composite glyph.

Reads composite glyph components repeatedly until the MORE_COMPONENTS flag
(0x0020) is not set in a component's flags. This allows composite glyphs to be
built from an arbitrary number of component glyphs.

Returns list of components in the order they appear (which matters for point
matching and rendering).
-}
parseCompositeComponentsP :: Get [CompositeGlyphData]
parseCompositeComponentsP = label "compositeComponents" $ go []
  where
    go acc = do
      component <- parseCompositeComponentP
      let flags = cgFlags component
      if flags .&. moreComponentsFlag /= 0
        then go (component : acc)
        else return (reverse (component : acc))

{-|
Parse a composite glyph.

Reads all components of a composite glyph and optional instructions:

1. Parse all components until MORE_COMPONENTS flag is clear
2. If WE_HAVE_INSTRUCTIONS flag is set on last component:
   - Read instruction length (Word16)
   - Read instruction bytes
   - Attach instructions to last component

The instructions apply to the entire composite glyph after all components are
assembled. They're stored on the last component for convenience.

@_header@: Glyph header (not currently used but provided for consistency)

Returns list of composite components with instructions on last component if
present.
-}
compositeGlyphP :: GlyphHeader -> Get [CompositeGlyphData]
compositeGlyphP _header = label "compositeGlyph" $ do
  components <- parseCompositeComponentsP

  -- Check if we have instructions (look at last component's flags)
  let lastFlags = if null components then 0 else cgFlags (last components)

  instructions <- if lastFlags .&. weHaveInstructionsFlag /= 0
    then do
      instructionLength <- getWord16be
      getByteString (fromIntegral instructionLength)
    else return ""

  -- Update the last component with instructions if present
  return $ if null components || instructions == ""
    then components
    else init components ++ [(last components) { cgInstructions = instructions }]

{-|
Parse a single complete glyph.

Determines the glyph type based on the number of contours in the header and
dispatches to the appropriate parser:

* Input empty: 'EmptyGlyph'
* numberOfContours == 0: 'EmptyGlyph'
* numberOfContours > 0: 'SimpleGlyph' with that many contours
* numberOfContours < 0: 'CompositeGlyph' (value -1 indicates composite)

This is the primary parser to use when parsing individual glyphs from byte
ranges determined by the loca table.

Returns a complete glyph of the appropriate type.
-}
singleGlyphP :: Get Glyph
singleGlyphP = label "singleGlyph" $ do
  empty <- isEmpty
  if empty
    then return EmptyGlyph
    else do
      header <- glyphHeaderP
      case ghNumberOfContours header of
        0 -> return EmptyGlyph
        n | n > 0 -> do
          glyphData <- simpleGlyphP header
          return (SimpleGlyph header glyphData)
        _ -> do  -- n < 0, composite glyph
          components <- compositeGlyphP header
          return (CompositeGlyph header components)

{-|
Parse a complete glyph table.

Note: The glyf table cannot be correctly parsed without the loca (location)
table, which specifies the offset and length of each glyph. The raw glyf table
data is just a concatenation of variable-length glyph descriptions with no
delimiters or length fields.

This function currently returns an empty GlyphTable as a placeholder.

To properly parse glyphs:

1. Parse the loca table to get offsets
2. For each glyph i, extract bytes from offset[i] to offset[i+1]
3. Use 'singleGlyphP' to parse each glyph's byte range
4. Collect results into a GlyphTable

See 'singleGlyphP' for parsing individual glyphs.
-}
glyphP :: Get GlyphTable
glyphP = label "glyphTable" $ do
  -- Cannot parse glyf table without loca table offsets
  -- Return empty table for now
  return (GlyphTable [])

