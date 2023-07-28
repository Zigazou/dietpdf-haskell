{-# LANGUAGE OverloadedStrings #-}
module Command.Info
  ( showInfo
  ) where

import qualified Data.Text.Lazy                as T
import           Formatting                     ( (%)
                                                , format
                                                , int
                                                )
import           Pdf.Document.Document          ( PDFDocument )
import           Pdf.Object.Object              ( objectInfo )
import           Util.UnifiedError              ( FallibleT )
import           Util.Logging                   ( sayF )

showInfo :: PDFDocument -> FallibleT IO ()
showInfo document = do
  sayF . T.toStrict $ format ("Found " % int % " objects") (length document)
  mapM_ (sayF . T.toStrict . objectInfo) document
