{-# LANGUAGE OverloadedStrings   #-}

-- | This modules contains functions to help dealing with Text strings.
module Pdf.Object.Format
  ( txtObjectNumberVersion
  ) where

import qualified Data.Text                     as T
import           Util.Text                      ( txtNumberVersion )
import           Pdf.Object.Object              ( PDFObject(PDFIndirectObject) )

txtObjectNumberVersion :: PDFObject -> T.Text
txtObjectNumberVersion (PDFIndirectObject number version _ _) =
  txtNumberVersion number version
txtObjectNumberVersion _ = "anonymous"
