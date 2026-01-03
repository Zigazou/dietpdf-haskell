{-|
Text formatting utilities for PDF objects

This module provides utility functions for formatting PDF objects as text,
particularly for object identification and debugging purposes.
-}
module PDF.Object.Format
  ( txtObjectNumberVersion
  ) where

import Data.Text qualified as T

import PDF.Object.Object
    ( PDFObject (PDFArray, PDFBool, PDFComment, PDFDictionary, PDFEndOfFile, PDFHexString, PDFIndirectObject, PDFIndirectObjectWithGraphics, PDFIndirectObjectWithStream, PDFKeyword, PDFName, PDFNull, PDFNumber, PDFObjectStream, PDFReference, PDFStartXRef, PDFString, PDFTrailer, PDFVersion, PDFXRef, PDFXRefStream)
    )

import Util.Text (txtNumberVersion)

{-|
Format a PDF object as its object number and version, or an anonymous label.

For indirect objects (those with object number and generation number), returns a
formatted string like "42 0" representing the object number and version.

For direct (non-indirect) objects, returns "anonymous" followed by the object
type in parentheses, such as:

- "anonymous (PDFComment)"
- "anonymous (PDFNumber)"
- "anonymous (PDFString)"
- "anonymous (PDFArray)"
- etc.

This is useful for debugging and logging to identify which objects are indirect
(named) versus inline (anonymous) and to display their object references.

__Parameters:__

- A PDF object (indirect, direct, or special structure)

__Returns:__ A text representation of the object's identity.
-}
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
