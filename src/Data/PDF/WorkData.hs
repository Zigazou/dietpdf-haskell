module Data.PDF.WorkData
  ( WorkData(WorkData, wPDF, wContexts, wNameTranslations)
  , emptyWorkData
  ) where

import Data.ByteString (ByteString)
import Data.Context (Context)
import Data.Kind (Type)
import Data.PDF.PDFPartition (PDFPartition)
import Data.TranslationTable (TranslationTable)

type WorkData :: Type
data WorkData = WorkData
  { wPDF              :: !PDFPartition
  , wContexts         :: ![Context]
  , wNameTranslations :: !(TranslationTable ByteString)
  }

emptyWorkData :: WorkData
emptyWorkData = WorkData mempty [] mempty
