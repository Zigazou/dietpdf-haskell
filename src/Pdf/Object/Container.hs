module Pdf.Object.Container
  ( deepMap
  ) where

import qualified Data.HashMap.Strict           as HM
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFIndirectObject
                                                  , PDFDictionary
                                                  , PDFArray
                                                  )
                                                )

deepMap :: (PDFObject -> PDFObject) -> PDFObject -> PDFObject
deepMap fn (PDFIndirectObject number revision object stream) =
  PDFIndirectObject number revision (deepMap fn object) stream
deepMap fn (PDFDictionary dictionary) =
  PDFDictionary (HM.map (deepMap fn) dictionary)
deepMap fn (PDFArray items) = PDFArray (deepMap fn <$> items)
deepMap fn object           = fn object
