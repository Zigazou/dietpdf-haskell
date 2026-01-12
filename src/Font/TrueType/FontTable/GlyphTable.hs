{-|
TrueType glyph table (glyf) representation and manipulation.

This module provides data structures and functions for working with the TrueType
glyph table, which stores the actual outline data for each glyph in the font.
The glyph table is one of the most important tables in a TrueType font,
containing the visual representation of characters.

== Glyph Types

Glyphs come in three varieties:

* 'EmptyGlyph': Glyphs with no outline (e.g., space character)
* 'SimpleGlyph': Single outline defined by contours and points
* 'CompositeGlyph': Built from multiple component glyphs with transformations

== Structure

Each glyph consists of:

* A header with bounding box and contour count
* The glyph data (outline points for simple, components for composite)
* Optional hinting instructions for grid-fitting at small sizes

== Operations

The module supports:

* Serialization to binary format ('fromGlyphTable')
* Removing hinting instructions ('removeHinting')
* Generating the corresponding location table ('generateLocationTable')

The location table (loca) is required alongside the glyph table to indicate
where each glyph starts within the serialized glyph data.
-}
module Font.TrueType.FontTable.GlyphTable
  ( GlyphTable
      ( GlyphTable
      , gtGlyphs
      )
  , Glyph
      ( SimpleGlyph
      , CompositeGlyph
      , EmptyGlyph
      )
  , SimpleGlyphData
      ( SimpleGlyphData
      , sgEndPtsOfContours
      , sgInstructionLength
      , sgInstructions
      , sgFlags
      , sgXCoordinates
      , sgYCoordinates
      )
  , CompositeGlyphData
      ( CompositeGlyphData
      , cgFlags
      , cgGlyphIndex
      , cgArgument1
      , cgArgument2
      , cgTransformation
      , cgInstructions
      )
  , GlyphHeader
      ( GlyphHeader
      , ghNumberOfContours
      , ghXMin
      , ghYMin
      , ghXMax
      , ghYMax
      )
  , TransformationMatrix(..)
  , fromGlyphTable
  , removeHinting
  , generateLocationTable
  ) where

import Data.Binary.Put (PutM, putInt16be, putWord16be, putWord8, runPut)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Int (Int16)
import Data.Kind (Type)
import Data.Word (Word16, Word8)

import Font.TrueType.FontTable.LocationTable (LocationTable (LocationTable))

{-|
TrueType glyph table representation containing all glyphs in the font.

The glyph table (glyf) stores the outline data for all glyphs in the font.
Glyphs are stored sequentially and referenced by index. The companion location
table (loca) provides offsets to find each glyph within the serialized data.

The order of glyphs must match the order expected by other font tables
(particularly the character-to-glyph mapping table, cmap).
-}
type GlyphTable :: Type
newtype GlyphTable = GlyphTable
  { gtGlyphs :: [Glyph] -- ^ List of all glyphs in the font
  }
  deriving stock (Eq, Show)

{-|
Header present in all glyph descriptions.

The glyph header appears at the start of both simple and composite glyph
descriptions. It provides the bounding box and indicates whether the glyph is
simple (positive contour count) or composite (negative contour count).

For composite glyphs, 'ghNumberOfContours' is set to -1. For simple glyphs, it
indicates the number of separate contours in the outline.
-}
type GlyphHeader :: Type
data GlyphHeader = GlyphHeader
  { ghNumberOfContours :: Int16  -- ^ Number of contours (negative for composite)
  , ghXMin             :: Int16  -- ^ Minimum x for coordinate data
  , ghYMin             :: Int16  -- ^ Minimum y for coordinate data
  , ghXMax             :: Int16  -- ^ Maximum x for coordinate data
  , ghYMax             :: Int16  -- ^ Maximum y for coordinate data
  }
  deriving stock (Eq, Show)

{-|
Data for a simple glyph (single glyph outline).

A simple glyph consists of one or more contours, where each contour is a closed
path made up of points. Points can be on-curve (on the path) or off-curve
(control points for quadratic Bézier curves).

The arrays must satisfy: length(sgFlags) == length(sgXCoordinates) ==
length(sgYCoordinates)

The 'sgEndPtsOfContours' array has one entry per contour, indicating the index
of the last point in that contour.
-}
type SimpleGlyphData :: Type
data SimpleGlyphData = SimpleGlyphData
  { sgEndPtsOfContours  :: [Word16]    -- ^ Array of last points of each contour
  , sgInstructionLength :: Word16      -- ^ Length of instruction array
  , sgInstructions      :: ByteString  -- ^ Instructions for this glyph
  , sgFlags             :: [Word8]     -- ^ Array of flags
  , sgXCoordinates      :: [Int16]     -- ^ Array of x-coordinates
  , sgYCoordinates      :: [Int16]     -- ^ Array of y-coordinates
  }
  deriving stock (Eq, Show)

{-|
Transformation matrix for composite glyphs.

Defines how a component glyph should be scaled or transformed when used in a
composite glyph. The transformation is applied before positioning.

* 'NoScale': Identity transformation, no scaling
* 'UniformScale': Scale uniformly in both X and Y directions
* 'XYScale': Different scale factors for X and Y axes
* 'Transform2x2': Full 2x2 affine transformation matrix [a b; c d]

The transformation matrix applies as: [x' y'] = [x y] * [a b; c d]
-}
type TransformationMatrix :: Type
data TransformationMatrix
  = NoScale                           -- ^ No transformation
  | UniformScale Int16                -- ^ Uniform scaling (scale value)
  | XYScale Int16 Int16               -- ^ X and Y scale factors
  | Transform2x2 Int16 Int16 Int16 Int16  -- ^ Full 2x2 transformation matrix
  deriving stock (Eq, Show)

{-|
Data for a composite glyph (glyph built from other glyphs).

A composite glyph component references another glyph by index and applies a
transformation and offset to it. The flags determine how arguments are
interpreted:

* If ARG_1_AND_2_ARE_WORDS flag is set, arguments are 16-bit values
* If ARGS_ARE_XY_VALUES flag is set, arguments are X/Y offsets; otherwise
  they're point numbers for matching

Multiple components are combined to form the final composite glyph outline.
-}
type CompositeGlyphData :: Type
data CompositeGlyphData = CompositeGlyphData
  { cgFlags          :: Word16              -- ^ Component flags
  , cgGlyphIndex     :: Word16              -- ^ Glyph index of component
  , cgArgument1      :: Int16               -- ^ First argument (offset or point)
  , cgArgument2      :: Int16               -- ^ Second argument (offset or point)
  , cgTransformation :: TransformationMatrix -- ^ Transformation for this component
  , cgInstructions   :: ByteString          -- ^ Optional instructions
  }
  deriving stock (Eq, Show)

{-|
Representation of a single glyph in the glyf table.

Glyphs can be:

* 'EmptyGlyph': No outline data (used for whitespace characters like space)
* 'SimpleGlyph': Single outline with contours defined by points
* 'CompositeGlyph': Built from one or more component glyphs with transformations

The choice between simple and composite is typically made based on whether the
glyph can be efficiently represented by combining existing glyphs (e.g.,
accented characters built from base + accent).
-}
type Glyph :: Type
data Glyph
  = EmptyGlyph                                           -- ^ Empty glyph (no outline)
  | SimpleGlyph GlyphHeader SimpleGlyphData              -- ^ Simple glyph with single outline
  | CompositeGlyph GlyphHeader [CompositeGlyphData]      -- ^ Composite glyph (multiple components)
  deriving stock (Eq, Show)

{-|
Serialize a GlyphHeader to binary format.

Writes 10 bytes in big-endian format:

* 2 bytes: numberOfContours (Int16)
* 2 bytes: xMin (Int16)
* 2 bytes: yMin (Int16)
* 2 bytes: xMax (Int16)
* 2 bytes: yMax (Int16)
-}
putGlyphHeader :: GlyphHeader -> PutM ()
putGlyphHeader header = do
  putInt16be (ghNumberOfContours header)
  putInt16be (ghXMin header)
  putInt16be (ghYMin header)
  putInt16be (ghXMax header)
  putInt16be (ghYMax header)

{-|
Serialize simple glyph data to binary format.

Writes the glyph outline data in the TrueType specification format:

1. End points of contours array (Word16 per entry)
2. Instruction length (Word16)
3. Instructions bytes
4. Flags array (Word8 per point)
5. X-coordinates array (Int16 per point)
6. Y-coordinates array (Int16 per point)

Note: In actual TrueType format, coordinates and flags can be compressed, but
this implementation uses the uncompressed form.
-}
putSimpleGlyphData :: SimpleGlyphData -> PutM ()
putSimpleGlyphData glyph = do
  mapM_ putWord16be (sgEndPtsOfContours glyph)
  putWord16be (sgInstructionLength glyph)
  mapM_ putWord8 (BS.unpack $ sgInstructions glyph)
  mapM_ putWord8 (sgFlags glyph)
  mapM_ putInt16be (sgXCoordinates glyph)
  mapM_ putInt16be (sgYCoordinates glyph)

{-|
Serialize transformation matrix to binary format.

Writes the transformation data according to the matrix type:

* 'NoScale': No bytes written (identity transformation)
* 'UniformScale': 2 bytes (single Int16 scale factor)
* 'XYScale': 4 bytes (two Int16 scale factors)
* 'Transform2x2': 8 bytes (four Int16 values: a, b, c, d)

The component flags must be set appropriately to indicate which transformation
type is present.
-}
putTransformationMatrix :: TransformationMatrix -> PutM ()
putTransformationMatrix NoScale           = return ()
putTransformationMatrix (UniformScale s)  = putInt16be s
putTransformationMatrix (XYScale x y)     = putInt16be x >> putInt16be y
putTransformationMatrix (Transform2x2 a b c d) = do
  putInt16be a
  putInt16be b
  putInt16be c
  putInt16be d

{-|
Serialize composite glyph component to binary format.

Writes a single component of a composite glyph:

1. Flags (Word16) - indicates transformation type and argument interpretation
2. Glyph index (Word16) - which glyph to use as component
3. Argument1 (Int16) - X offset or point number
4. Argument2 (Int16) - Y offset or point number
5. Transformation matrix (0-8 bytes, depending on flags)
6. Instructions (if present and MORE_COMPONENTS flag not set)

The flags determine the presence and interpretation of transformation data.
-}
putCompositeGlyphData :: CompositeGlyphData -> PutM ()
putCompositeGlyphData component = do
  putWord16be (cgFlags component)
  putWord16be (cgGlyphIndex component)
  putInt16be (cgArgument1 component)
  putInt16be (cgArgument2 component)
  putTransformationMatrix (cgTransformation component)
  mapM_ putWord8 (BS.unpack $ cgInstructions component)

{-|
Serialize a single glyph to binary format.

Produces the binary representation according to glyph type:

* 'EmptyGlyph': No bytes (zero-length glyph)
* 'SimpleGlyph': Header + simple glyph data
* 'CompositeGlyph': Header + all component data

The serialized data is suitable for inclusion in the glyf table.
-}
putGlyph :: Glyph -> PutM ()
putGlyph EmptyGlyph = return ()
putGlyph (SimpleGlyph header glyphData) = do
  putGlyphHeader header
  putSimpleGlyphData glyphData
putGlyph (CompositeGlyph header components) = do
  putGlyphHeader header
  mapM_ putCompositeGlyphData components

{-|
Serialize a GlyphTable to binary ByteString.

Converts all glyphs in the table to the binary format specified by the TrueType
font specification.
-}
fromGlyphTable :: GlyphTable -> ByteString
fromGlyphTable glyphTable =
  BSL.toStrict $ runPut $ mapM_ putGlyph (gtGlyphs glyphTable)

{-|
Remove all hinting instructions from a GlyphTable.

This function removes TrueType hinting instructions from all glyphs:
- For simple glyphs: clears the instruction array and sets length to 0
- For composite glyphs: clears instructions from all components
- Empty glyphs remain unchanged
-}
removeHinting :: GlyphTable -> GlyphTable
removeHinting (GlyphTable glyphs) = GlyphTable (removeGlyphHinting <$> glyphs)
  where
    -- Remove hinting from a single glyph
    removeGlyphHinting :: Glyph -> Glyph
    removeGlyphHinting EmptyGlyph = EmptyGlyph
    removeGlyphHinting (SimpleGlyph header glyphData) =
      SimpleGlyph header glyphData
        { sgInstructionLength = 0
        , sgInstructions = BS.empty
        }
    removeGlyphHinting (CompositeGlyph header components) =
      CompositeGlyph header (removeComponentHinting <$> components)

    -- Remove instructions from a composite glyph component
    removeComponentHinting :: CompositeGlyphData -> CompositeGlyphData
    removeComponentHinting comp = comp { cgInstructions = BS.empty }

{-|
Generate a LocationTable from a GlyphTable.

Calculates the offset of each glyph relative to the start of the glyf table. The
offsets are cumulative based on the serialized size of each glyph. The location
table will have n+1 entries for n glyphs, with the last entry indicating the
total size of the glyf table.
-}
generateLocationTable :: GlyphTable -> LocationTable
generateLocationTable (GlyphTable glyphs) =
  LocationTable (scanl (+) 0 glyphSizes)
  where
    glyphSizes = map (fromIntegral . BS.length . glyphToByteString) glyphs

    -- Convert a single glyph to its binary representation for size calculation
    glyphToByteString :: Glyph -> ByteString
    glyphToByteString glyph = BSL.toStrict $ runPut $ putGlyph glyph
