module Font.TrueType.Parser.TableIdentifier
  ( tableIdentifierP
  ) where

import           Font.TrueType.TableIdentifier  ( TableIdentifier
                                                , toTableIdentifier
                                                )

import           Data.Binary.Parser             ( Get )
import           Data.Binary.Get.Internal       ( getByteString )

tableIdentifierP :: Get TableIdentifier
tableIdentifierP = toTableIdentifier <$> getByteString 4
