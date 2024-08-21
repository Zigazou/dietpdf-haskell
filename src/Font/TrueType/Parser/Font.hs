module Font.TrueType.Parser.Font
  ( ttfParse
  , fontDirectoryP
  ) where

import Control.Monad (when)

import Data.Binary (get)
import Data.Binary.Parser (Get, label, parseOnly)
import Data.ByteString qualified as BS
import Data.Sequence qualified as SQ

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

import Util.Array (Array)
import Util.UnifiedError (UnifiedError (UnknownScalerType))

offsetSubtableP :: Get OffsetSubtable
offsetSubtableP =
  OffsetSubtable <$> scalerTypeP <*> get <*> get <*> get <*> get

tableEntryP :: Get TableEntry
tableEntryP =
  TableEntry <$> tableIdentifierP <*> get <*> get <*> get <*> pure (FTRaw "")

readNTableEntry :: Int -> Get (Array TableEntry)
readNTableEntry 0 = fail ""
readNTableEntry n = do
  entry <- tableEntryP
  if n == 1
    then return (SQ.singleton entry)
    else do
      entries <- readNTableEntry (n - 1)
      return (entry SQ.:<| entries)

fontDirectoryP :: Get FontDirectory
fontDirectoryP = label "fontDirectory" $ do
  subtable <- offsetSubtableP
  when (isUnknown $ osScalerType subtable) (fail "Unknown scaler type")
  entries <- readNTableEntry (fromIntegral $ osNumTables subtable)
  return $ FontDirectory subtable entries

{-|
Parses a True Type font file from a bytestring.
-}
ttfParse
  :: BS.ByteString -- ^ The bytestring to parse coming from a file.
  -> Either UnifiedError FontDirectory -- ^ Error or a `FontDirectory`.
ttfParse fontfile = case parseOnly fontDirectoryP fontfile of
  Left _anyError -> Left (UnknownScalerType "")
  Right (FontDirectory subtable directory) ->
    Right (FontDirectory subtable (loadContent fontfile <$> directory))
