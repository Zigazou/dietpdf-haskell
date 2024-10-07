module Pdf.Graphics.RenameResources
  ( renameResourcesInObject
  ) where

import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.Map qualified as Map
import Data.TranslationTable (TranslationTable, convert)

import Pdf.Graphics.Object (GFXObject (GFXArray, GFXDictionary, GFXName))

import Util.Dictionary (Dictionary)

renameResourcesInDictionary
  :: TranslationTable ByteString
  -> Dictionary GFXObject
  -> Dictionary GFXObject
renameResourcesInDictionary table
  = Map.fromList
  . fmap (bimap (convert table) (renameResourcesInObject table))
  . Map.toList

renameResourcesInObject
  :: TranslationTable ByteString
  -> GFXObject
  -> GFXObject
renameResourcesInObject table (GFXName name) =
  GFXName (convert table name)
renameResourcesInObject table (GFXArray objects) =
  GFXArray (renameResourcesInObject table <$> objects)
renameResourcesInObject table (GFXDictionary dictionary) =
  GFXDictionary (renameResourcesInDictionary table dictionary)
renameResourcesInObject _table object = object
