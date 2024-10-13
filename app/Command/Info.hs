module Command.Info
  ( showInfo
  ) where

import Control.Monad (forM_)

import Data.Fallible (FallibleT)
import Data.PDF.ObjectInfo
    ( ObjectInfo (oCategory, oDescription, oNumber, oOffset, oStream)
    , StreamInfo (sFilteredSize, sUnfilteredSize)
    )
import Data.PDF.PDFDocument (PDFDocument)
import Data.PDF.PDFWork (evalPDFWork)
import Data.Text qualified as T

import PDF.Processing.ObjectInfo (objectInfo)

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
    info <- evalPDFWork (objectInfo object Nothing)

    disp
      [ show (oCategory info)
      , maybe "" show (oNumber info)
      , maybe "" show (oOffset info)
      , maybe "" (show . sFilteredSize) (oStream info)
      , maybe "" (show . sUnfilteredSize) (oStream info)
      , T.unpack (oDescription info)
      ]
