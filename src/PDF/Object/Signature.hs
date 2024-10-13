module PDF.Object.Signature
  ( streamHash
  ) where

import Crypto.Hash (Digest, SHA512, hash)

import PDF.Object.Object (PDFObject (PDFIndirectObjectWithStream))

streamHash :: PDFObject -> Maybe (Digest SHA512)
streamHash (PDFIndirectObjectWithStream _ _ _ stream) = return $ hash stream
streamHash _anyOtherObject                            = Nothing
