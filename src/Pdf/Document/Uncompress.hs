module Pdf.Document.Uncompress
  ( uncompress
  ) where

import           Pdf.Document.Document          ( PDFDocument
                                                , toList
                                                , fromList
                                                )
import           Pdf.Document.ObjectStream      ( explode )
import           Pdf.Object.Unfilter            ( unfilter )
import           Util.Logging                   ( Logging )
import           Data.Functor                   ( (<&>) )
import           Util.UnifiedError              ( FallibleT )

{- |
Uncompress all `PDFObject` contained in a `PDFDocument`.

Objects embedded in object streams are extracted and the object stream is
removed.

If a `PDFObject` cannot be uncompressed (meaning its processing generated an
error), the object is left as is. Thus this function may leave object
uncompressed.
-}
uncompress :: Logging m => PDFDocument -> FallibleT m PDFDocument
uncompress pdf = do
  objects <- explode pdf <&> toList
  fromList <$> sequence (unfilter <$> objects)
