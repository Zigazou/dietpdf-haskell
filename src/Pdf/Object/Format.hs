{-# LANGUAGE OverloadedStrings   #-}

-- | This modules contains functions to help dealing with Text strings.
module Pdf.Object.Format
  ( txtObjectNumberVersion
  ) where

import qualified Data.Text                     as T
import           Util.Text                      ( txtNumberVersion )
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFIndirectObject
                                                  , PDFIndirectObjectWithStream
                                                  , PDFIndirectObjectWithGraphics
                                                  , PDFXRef
                                                  , PDFStartXRef
                                                  , PDFTrailer
                                                  , PDFNull
                                                  , PDFBool
                                                  , PDFObjectStream
                                                  , PDFDictionary
                                                  , PDFArray
                                                  , PDFReference
                                                  , PDFHexString
                                                  , PDFString
                                                  , PDFName
                                                  , PDFKeyword
                                                  , PDFNumber
                                                  , PDFEndOfFile
                                                  , PDFVersion
                                                  , PDFComment
                                                  )
                                                )

txtObjectNumberVersion :: PDFObject -> T.Text
txtObjectNumberVersion (PDFIndirectObject number version _) =
  txtNumberVersion number version
txtObjectNumberVersion (PDFIndirectObjectWithStream number version _ _) =
  txtNumberVersion number version
txtObjectNumberVersion (PDFIndirectObjectWithGraphics number version _ _) =
  txtNumberVersion number version
txtObjectNumberVersion (PDFObjectStream number version _ _) =
  txtNumberVersion number version
txtObjectNumberVersion PDFComment{}    = "anonymous (PDFComment)"
txtObjectNumberVersion PDFVersion{}    = "anonymous (PDFVersion)"
txtObjectNumberVersion PDFEndOfFile{}  = "anonymous (PDFEndOfFile)"
txtObjectNumberVersion PDFNumber{}     = "anonymous (PDFNumber)"
txtObjectNumberVersion PDFKeyword{}    = "anonymous (PDFKeyword)"
txtObjectNumberVersion PDFName{}       = "anonymous (PDFName)"
txtObjectNumberVersion PDFString{}     = "anonymous (PDFString)"
txtObjectNumberVersion PDFHexString{}  = "anonymous (PDFHexString)"
txtObjectNumberVersion PDFReference{}  = "anonymous (PDFReference)"
txtObjectNumberVersion PDFArray{}      = "anonymous (PDFArray)"
txtObjectNumberVersion PDFDictionary{} = "anonymous (PDFDictionary)"
txtObjectNumberVersion PDFBool{}       = "anonymous (PDFBool)"
txtObjectNumberVersion PDFNull{}       = "anonymous (PDFNull)"
txtObjectNumberVersion PDFXRef{}       = "anonymous (PDFXRef)"
txtObjectNumberVersion PDFTrailer{}    = "anonymous (PDFTrailer)"
txtObjectNumberVersion PDFStartXRef{}  = "anonymous (PDFStartXRef)"
