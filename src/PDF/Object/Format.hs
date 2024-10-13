-- | This modules contains functions to help dealing with Text strings.
module PDF.Object.Format
  ( txtObjectNumberVersion
  ) where

import Data.Text qualified as T

import PDF.Object.Object
    ( PDFObject (PDFArray, PDFBool, PDFComment, PDFDictionary, PDFEndOfFile, PDFHexString, PDFIndirectObject, PDFIndirectObjectWithGraphics, PDFIndirectObjectWithStream, PDFKeyword, PDFName, PDFNull, PDFNumber, PDFObjectStream, PDFReference, PDFStartXRef, PDFString, PDFTrailer, PDFVersion, PDFXRef, PDFXRefStream)
    )

import Util.Text (txtNumberVersion)

txtObjectNumberVersion :: PDFObject -> T.Text
txtObjectNumberVersion (PDFIndirectObject number version _) =
  txtNumberVersion number version
txtObjectNumberVersion (PDFIndirectObjectWithStream number version _ _) =
  txtNumberVersion number version
txtObjectNumberVersion (PDFIndirectObjectWithGraphics number version _ _) =
  txtNumberVersion number version
txtObjectNumberVersion (PDFObjectStream number version _ _) =
  txtNumberVersion number version
txtObjectNumberVersion (PDFXRefStream number version _ _) =
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
