module Pdf.Object.Signature
  ( streamHash
  ) where

import           Crypto.Hash                    ( hash
                                                , Digest
                                                , SHA512
                                                )
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFIndirectObjectWithStream
                                                  )
                                                )

streamHash :: PDFObject -> Maybe (Digest SHA512)
streamHash (PDFIndirectObjectWithStream _ _ _ stream) = return $ hash stream
streamHash _anyOtherObject                            = Nothing
