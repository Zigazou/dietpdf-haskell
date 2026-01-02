{-|
Parse TrueType font table identifiers.

Table identifiers are 4-byte ASCII tags that uniquely identify specific font
tables within a font file (e.g., "head", "glyf", "cmap", etc.).
-}
module Font.TrueType.Parser.TableIdentifier
  ( tableIdentifierP
  ) where

import Data.Binary.Get.Internal (getByteString)
import Data.Binary.Parser (Get)

import Font.TrueType.TableIdentifier (TableIdentifier, toTableIdentifier)

{-|
Parse a 4-byte table identifier.

Reads exactly 4 bytes and converts them to a 'TableIdentifier' value via
'toTableIdentifier'. The table identifier is an ASCII tag that specifies which
font table (e.g., head, glyf, cmap) is being referenced.
-}
tableIdentifierP :: Get TableIdentifier
tableIdentifierP = toTableIdentifier <$> getByteString 4
