module Pdf.Object.Object.RenameResources
  ( renameResources
  ) where

import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.Map qualified as Map
import Data.PDF.PDFObject
    ( PDFObject (PDFArray, PDFDictionary, PDFIndirectObject, PDFIndirectObjectWithStream, PDFName, PDFObjectStream, PDFTrailer, PDFXRefStream)
    )
import Data.TranslationTable (TranslationTable, convert)

import Util.Dictionary (Dictionary)


renameResourcesInDictionary
  :: TranslationTable ByteString
  -> Dictionary PDFObject
  -> Dictionary PDFObject
renameResourcesInDictionary table
  = Map.fromList
  . fmap (bimap (convert table) (renameResources table))
  . Map.toList

renameResources
  :: TranslationTable ByteString
  -> PDFObject
  -> PDFObject
renameResources table (PDFName name) = PDFName (convert table name)
renameResources table (PDFArray objects) =
  PDFArray (renameResources table <$> objects)
renameResources table (PDFDictionary dictionary) =
  PDFDictionary (renameResourcesInDictionary table dictionary)
renameResources table (PDFIndirectObject number revision (PDFDictionary dict)) =
  PDFIndirectObject number revision
                    (PDFDictionary (renameResourcesInDictionary table dict))
renameResources table (PDFIndirectObjectWithStream number revision dict stream) =
  PDFIndirectObjectWithStream number revision
                              (renameResourcesInDictionary table dict)
                              stream
renameResources table (PDFObjectStream number revision dict stream) =
  PDFObjectStream number revision
                  (renameResourcesInDictionary table dict)
                  stream
renameResources table (PDFXRefStream number revision dict stream) =
  PDFXRefStream number revision
                (renameResourcesInDictionary table dict)
                stream
renameResources table (PDFTrailer (PDFDictionary dict)) =
  PDFTrailer (PDFDictionary (renameResourcesInDictionary table dict))
renameResources _table object = object
