module Command.Info
  ( showInfo
  ) where

import Control.Monad (forM_)

import Data.Fallible (FallibleT)
import Data.ObjectInfo
    ( ObjectInfo (oCategory, oDescription, oNumber, oOffset, oStream)
    , StreamInfo (sFilteredSize, sUnfilteredSize)
    )
import Data.Text qualified as T

import Pdf.Document.Document (PDFDocument)
import Pdf.Object.ObjectInfo (objectInfo)

import Util.Display (disp)

header :: [String]
header = [ "type"
         , "id"
         , "offset"
         , "compressed"
         , "uncompressed"
         , "description"
         ]

showInfo :: PDFDocument -> FallibleT IO ()
showInfo document = do
  disp header

  forM_ document $ \object -> do
    info <- objectInfo object Nothing

    disp
      [ show (oCategory info)
      , maybe "" show (oNumber info)
      , maybe "" show (oOffset info)
      , maybe "" (show . sFilteredSize) (oStream info)
      , maybe "" (show . sUnfilteredSize) (oStream info)
      , T.unpack (oDescription info)
      ]
