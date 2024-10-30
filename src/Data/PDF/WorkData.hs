module Data.PDF.WorkData
  ( WorkData(WorkData, wPDF, wContexts, wNameTranslations, wSettings, wMasks)
  , emptyWorkData
  , setSettings
  ) where

import Data.Context (Context)
import Data.Kind (Type)
import Data.PDF.PDFPartition (PDFPartition)
import Data.PDF.Resource (Resource)
import Data.PDF.Settings (Settings, defaultSettings)
import Data.Set (Set)
import Data.TranslationTable (TranslationTable)

type WorkData :: Type
data WorkData = WorkData
  { wPDF              :: !PDFPartition
  , wContexts         :: ![Context]
  , wNameTranslations :: !(TranslationTable Resource)
  , wMasks            :: !(Set Int)
  , wSettings         :: !Settings
  }

emptyWorkData :: WorkData
emptyWorkData = WorkData mempty [] mempty mempty defaultSettings

setSettings :: WorkData -> Settings -> WorkData
setSettings workData settings = workData { wSettings = settings }
