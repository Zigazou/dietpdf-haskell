{-|
Parse TrueType font files from binary data.

Provides binary parsing for TrueType font directories and their constituent
table entries, with validation of scaler types and lazy loading of table
contents.
-}
module Font.TrueType.Parser.Font
  ( ttfParse
  , fontDirectoryP
  ) where

import Control.Monad (when)

import Data.Array (Array)
import Data.Binary (get)
import Data.Binary.Parser (Get, label, parseOnly)
import Data.ByteString (ByteString)
import Data.Fallible (Fallible)
import Data.Sequence qualified as SQ
import Data.UnifiedError (UnifiedError (UnknownScalerType))

import Font.TrueType.FontDirectory
    ( FontDirectory (FontDirectory)
    , OffsetSubtable (OffsetSubtable, osNumTables, osScalerType)
    , TableEntry (TableEntry)
    , loadContent
    )
import Font.TrueType.FontTable (FontTable (FTRaw))
import Font.TrueType.Parser.ScalerType (scalerTypeP)
import Font.TrueType.Parser.TableIdentifier (tableIdentifierP)
import Font.TrueType.ScalerType (isUnknown)

{-|
Parse a TrueType font offset subtable.

Reads the offset subtable header containing the scaler type, number of tables,
and search parameters for the table directory.
-}
offsetSubtableP :: Get OffsetSubtable
offsetSubtableP =
  OffsetSubtable <$> scalerTypeP <*> get <*> get <*> get <*> get

{-|
Parse a single font table entry from the directory.

Reads the table identifier, checksum, offset, and length. Table content is
initialized as empty and loaded later via 'loadContent'.
-}
tableEntryP :: Get TableEntry
tableEntryP =
  TableEntry <$> tableIdentifierP <*> get <*> get <*> get <*> pure (FTRaw "")

{-|
Recursively parse n table entries from the font directory.

Fails if @n@ is 0 (at least one table entry is required). Returns a sequence of
'TableEntry' values.
-}
readNTableEntry :: Int -> Get (Array TableEntry)
readNTableEntry 0 = fail ""
readNTableEntry n = do
  entry <- tableEntryP
  if n == 1
    then return (SQ.singleton entry)
    else do
      entries <- readNTableEntry (n - 1)
      return (entry SQ.:<| entries)

{-|
Parse a complete TrueType font directory.

Reads the offset subtable and all table entries, validating that the scaler type
is recognized. Fails if the scaler type is unknown or invalid.
-}
fontDirectoryP :: Get FontDirectory
fontDirectoryP = label "fontDirectory" $ do
  subtable <- offsetSubtableP
  when (isUnknown $ osScalerType subtable) (fail "Unknown scaler type")
  entries <- readNTableEntry (fromIntegral $ osNumTables subtable)
  return $ FontDirectory subtable entries

{-|
Parse a TrueType font file from raw binary data.

Decodes the font directory structure and lazily loads table contents from the
binary stream. Returns the fully populated 'FontDirectory' or an error if
parsing fails or the scaler type is unrecognized.

@ByteString@: Complete font file data read from disk @Fallible FontDirectory@:
Parsed directory with all tables loaded, or an error
-}
ttfParse
  :: ByteString -- ^ The bytestring to parse coming from a file.
  -> Fallible FontDirectory -- ^ Error or a `FontDirectory`.
ttfParse fontfile = case parseOnly fontDirectoryP fontfile of
  Left _anyError -> Left (UnknownScalerType "")
  Right (FontDirectory subtable directory) ->
    Right (FontDirectory subtable (loadContent fontfile <$> directory))
