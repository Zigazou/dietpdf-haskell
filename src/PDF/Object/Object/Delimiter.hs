{-|
Delimiter detection for PDF objects

This module provides functions to determine whether space delimiters are needed
between PDF objects during serialization. PDF syntax requires whitespace between
certain object types to ensure proper parsing.
-}
module PDF.Object.Object.Delimiter
  ( spaceIfNeeded
  ) where

import Data.ByteString (ByteString)
import Data.PDF.PDFObject
    ( PDFObject (PDFArray, PDFBool, PDFComment, PDFDictionary, PDFEndOfFile, PDFHexString, PDFIndirectObject, PDFIndirectObjectWithGraphics, PDFIndirectObjectWithStream, PDFKeyword, PDFName, PDFNull, PDFNumber, PDFObjectStream, PDFReference, PDFStartXRef, PDFString, PDFTrailer, PDFVersion, PDFXRef, PDFXRefStream)
    )


{-|
Test whether a PDF object ends with a delimiter character.

Some PDF objects naturally end with a delimiter (such as closing brackets,
parentheses, or structural markers) that prevents ambiguity when followed by
another object. Others (like numbers, keywords, and names) require explicit
whitespace separation to avoid being parsed as a single token.

Returns 'True' if the object ends with a delimiter when serialized to a
bytestring, 'False' otherwise.
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
Test whether a PDF object starts with a delimiter character.

Some PDF objects naturally begin with a delimiter (such as opening brackets,
parentheses, or the solidus prefix for names) that prevents ambiguity when
preceded by another object. Others (like numbers, keywords, and references)
require explicit whitespace separation from the previous object.

Returns 'True' if the object starts with a delimiter when serialized to a
bytestring, 'False' otherwise.
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
Determine whether whitespace is needed between two consecutive PDF objects.

When serializing multiple PDF objects to a bytestring, whitespace (typically a
single space) is needed between them only when neither object provides a
delimiter at their boundary. Specifically:

- If the first object ends with a delimiter, no space is needed
- Otherwise, if the second object starts with a delimiter, no space is needed
- Otherwise, a single space character is required

This ensures that the serialized output is valid PDF syntax without redundant
whitespace.

__Parameters:__

- The first PDF object
- The second PDF object

__Returns:__ An empty bytestring (@\"\"@) if no space is needed, or a single
space (@\" \"@) if separation is required.
-}
spaceIfNeeded :: PDFObject -> PDFObject -> ByteString
spaceIfNeeded object1 object2 | endsWithDelimiter object1   = ""
                              | startsWithDelimiter object2 = ""
                              | otherwise                   = " "
