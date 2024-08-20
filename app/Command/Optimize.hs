module Command.Optimize
  ( optimize
  ) where

import qualified Data.ByteString               as BS
import qualified Data.Text.Lazy.IO             as T
import           Formatting                     ( (%)
                                                , format
                                                , int
                                                )
import           Pdf.Document.Document          ( PDFDocument )
import           Pdf.Document.Encode            ( pdfEncode )
import           Util.UnifiedError              ( FallibleT )
import           Control.Monad.Trans.Class      ( MonadTrans(lift) )

optimize :: FilePath -> PDFDocument -> FallibleT IO ()
optimize outputPDF objects = do
  lift . T.putStrLn $ format ("Found " % int % " objects") (length objects)
  pdfEncode objects >>= lift . BS.writeFile outputPDF
