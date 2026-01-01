module PDF.Object.Object.Delimiter
  ( spaceIfNeeded
  ) where

import Data.ByteString (ByteString)
import Data.PDF.PDFObject
    ( PDFObject (PDFArray, PDFBool, PDFComment, PDFDictionary, PDFEndOfFile, PDFHexString, PDFIndirectObject, PDFIndirectObjectWithGraphics, PDFIndirectObjectWithStream, PDFKeyword, PDFName, PDFNull, PDFNumber, PDFObjectStream, PDFReference, PDFStartXRef, PDFString, PDFTrailer, PDFVersion, PDFXRef, PDFXRefStream)
    )


{-|
Indicates whether the `PDFObject` ends with a delimiter when converted to a
`ByteString`.
-}
endsWithDelimiter :: PDFObject -> Bool
endsWithDelimiter PDFComment{}                    = True
endsWithDelimiter PDFVersion{}                    = True
endsWithDelimiter PDFEndOfFile                    = True
endsWithDelimiter PDFNumber{}                     = False
endsWithDelimiter PDFKeyword{}                    = False
endsWithDelimiter PDFName{}                       = False
endsWithDelimiter PDFString{}                     = True
endsWithDelimiter PDFHexString{}                  = True
endsWithDelimiter PDFReference{}                  = False
endsWithDelimiter PDFArray{}                      = True
endsWithDelimiter PDFDictionary{}                 = True
endsWithDelimiter PDFIndirectObject{}             = True
endsWithDelimiter PDFIndirectObjectWithStream{}   = True
endsWithDelimiter PDFIndirectObjectWithGraphics{} = True
endsWithDelimiter PDFObjectStream{}               = True
endsWithDelimiter PDFXRefStream{}                 = True
endsWithDelimiter PDFBool{}                       = False
endsWithDelimiter PDFNull                         = False
endsWithDelimiter PDFXRef{}                       = False
endsWithDelimiter PDFTrailer{}                    = True
endsWithDelimiter PDFStartXRef{}                  = True

{-|
Indicates whether the `PDFObject` starts with a delimiter when converted to a
`ByteString`.
-}
startsWithDelimiter :: PDFObject -> Bool
startsWithDelimiter PDFComment{}                    = True
startsWithDelimiter PDFVersion{}                    = True
startsWithDelimiter PDFEndOfFile                    = True
startsWithDelimiter PDFNumber{}                     = False
startsWithDelimiter PDFKeyword{}                    = False
startsWithDelimiter PDFName{}                       = True
startsWithDelimiter PDFString{}                     = True
startsWithDelimiter PDFHexString{}                  = True
startsWithDelimiter PDFReference{}                  = False
startsWithDelimiter PDFArray{}                      = True
startsWithDelimiter PDFDictionary{}                 = True
startsWithDelimiter PDFIndirectObject{}             = False
startsWithDelimiter PDFIndirectObjectWithStream{}   = False
startsWithDelimiter PDFIndirectObjectWithGraphics{} = False
startsWithDelimiter PDFObjectStream{}               = False
startsWithDelimiter PDFXRefStream{}                 = False
startsWithDelimiter PDFBool{}                       = False
startsWithDelimiter PDFNull                         = False
startsWithDelimiter PDFXRef{}                       = False
startsWithDelimiter PDFTrailer{}                    = False
startsWithDelimiter PDFStartXRef{}                  = False

{-|
Tells if a space must be inserted between 2 `PDFObject` when converted to
`ByteString`.
-}
spaceIfNeeded :: PDFObject -> PDFObject -> ByteString
spaceIfNeeded object1 object2 | endsWithDelimiter object1   = ""
                              | startsWithDelimiter object2 = ""
                              | otherwise                   = " "
