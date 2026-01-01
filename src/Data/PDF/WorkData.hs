{-|
This module defines `WorkData`, the state container used throughout PDF
processing. It tracks the current PDF partition, contextual information for
logging, resource name translations, additional graphics states, masks, global
settings, and a progress index for reporting.

`WorkData` is typically threaded through the `PDFWork` monad to keep
transformations and inspections consistent and observable.
-}
module Data.PDF.WorkData
  ( WorkData(WorkData, wPDF, wContexts, wNameTranslations, wSettings, wMasks, wAdditionalGStates, wCurrentIndex)
  , emptyWorkData
  , setSettings
  , resetCurrentIndex
  , nextIndex
  ) where

import Data.Context (Context)
import Data.Kind (Type)
import Data.PDF.PDFPartition (PDFPartition)
import Data.PDF.Resource (Resource)
import Data.PDF.ResourceDictionary (ResourceDictionary)
import Data.PDF.Settings (Settings, defaultSettings)
import Data.Set (Set)
import Data.TranslationTable (TranslationTable)

{-|
WorkData encapsulates the state used during PDF processing, including the
current PDF partition, context stack, name translations, additional graphics
states, masks, settings, and the current index for generating new object
numbers.
-}
type WorkData :: Type
data WorkData = WorkData
  { wPDF               :: !PDFPartition                 -- ^ Current PDF partition under processing.
  , wContexts          :: ![Context]                    -- ^ Context stack used to prefix logs and errors.
  , wNameTranslations  :: !(TranslationTable Resource)  -- ^ Translation table for resource names.
  , wAdditionalGStates :: !ResourceDictionary           -- ^ Additional graphics states collected/registered.
  , wMasks             :: !(Set Int)                    -- ^ Set of object numbers considered as masks.
  , wSettings          :: !Settings                     -- ^ Global processing settings.
  , wCurrentIndex      :: !Int                          -- ^ Progress index for reporting operations.
  }

{-|
Initial empty `WorkData` used to bootstrap PDF processing.
-}
emptyWorkData :: WorkData
emptyWorkData = WorkData
  { wPDF = mempty
  , wContexts = []
  , wNameTranslations = mempty
  , wAdditionalGStates = mempty
  , wMasks = mempty
  , wSettings = defaultSettings
  , wCurrentIndex = 1
  }

{-|
Update the `Settings` contained in `WorkData`.
-}
setSettings :: WorkData -> Settings -> WorkData
setSettings workData settings = workData { wSettings = settings }

{-|
Reset the progress index to 1.
-}
resetCurrentIndex :: WorkData -> WorkData
resetCurrentIndex workData = workData { wCurrentIndex = 1 }

{-|
Increment the progress index by 1.
-}
nextIndex :: WorkData -> WorkData
nextIndex workData = workData { wCurrentIndex = wCurrentIndex workData + 1 }
