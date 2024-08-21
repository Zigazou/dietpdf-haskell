module Font.TrueType.Parser.TableIdentifier
  ( tableIdentifierP
  ) where

import Data.Binary.Get.Internal (getByteString)
import Data.Binary.Parser (Get)

import Font.TrueType.TableIdentifier (TableIdentifier, toTableIdentifier)

tableIdentifierP :: Get TableIdentifier
tableIdentifierP = toTableIdentifier <$> getByteString 4
