{-|
Cryptographic hashing utilities for PDF streams

This module provides cryptographic hashing functions for PDF stream objects.

Stream objects may contain binary data (images, compressed content, etc.) whose
integrity can be verified using cryptographic hashes. This module computes SHA512
digests of stream data for signature verification and integrity checking.
-}
module PDF.Object.Signature
  ( streamHash
  ) where

import Crypto.Hash (Digest, SHA512, hash)

import PDF.Object.Object (PDFObject (PDFIndirectObjectWithStream))

{-|
Compute the SHA512 hash of a PDF stream object's data.

Extracts the stream data from an indirect PDF object with stream and computes
its SHA512 cryptographic hash digest. Returns 'Nothing' if the object is not an
indirect object with stream data.

__Parameters:__

- A PDF object (may or may not be a stream)

__Returns:__ 'Just' the SHA512 digest if the object is an indirect object with
stream, or 'Nothing' if the object is not a stream object.

__Use cases:__

- Verifying stream data integrity
- Digital signature verification
- Content identification and deduplication
-}
streamHash :: PDFObject -> Maybe (Digest SHA512)
streamHash (PDFIndirectObjectWithStream _ _ _ stream) = return $ hash stream
streamHash _anyOtherObject                            = Nothing
