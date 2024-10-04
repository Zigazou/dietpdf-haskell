module Data.Logging
  ( Logging(say)
  , sayF
  , sayComparisonF
  , sayErrorF
  ) where

import Control.Monad (void)
import Control.Monad.Identity (Identity)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Writer (Writer, tell)

import Data.Context (Context (Context, NoContext))
import Data.Kind (Constraint, Type)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.UnifiedError (UnifiedError)

import System.IO (hFlush, stderr)

import Util.Number (round')

type Logging :: (Type -> Type) -> Constraint
class Monad m => Logging m where
  say :: Context -> T.Text -> m ()

-- This instance does not use the hPutStrLn function to ensure the output is
-- sent in a single write operation. This allows multiple threads to write to
-- the same handle without interleaving their output.
instance Logging IO where
  say :: Context -> T.Text -> IO ()
  say (Context intro) message = do
    TIO.hPutStr stderr (T.concat [intro, "\t", message, "\n"])
    hFlush stderr
  say NoContext message = do
    TIO.hPutStr stderr (T.concat [message, "\n"])
    hFlush stderr

instance Logging Identity where
  say :: Context -> T.Text -> Identity ()
  say _ _ = return ()

instance Logging (Writer [T.Text]) where
  say :: Context -> T.Text -> Writer [T.Text] ()
  say (Context intro) message = (void . tell . return)
                                (T.concat [intro, "\t", message])

  say NoContext message       = (void . tell . return)
                                message

sayF :: (Logging m, MonadTrans t) => Context -> T.Text -> t m ()
sayF mIntro = lift . say mIntro

sayComparisonF
  :: (Logging m, MonadTrans t)
  => Context
  -> T.Text
  -> Int
  -> Int
  -> t m ()
sayComparisonF intro label sizeBefore sizeAfter = sayF
  intro
  (T.concat
    [ label
    , "\t", T.pack (show sizeBefore)
    , "\t", T.pack (show sizeAfter)
    , "\t", if ratio > 0 then "+" else "", T.pack (show (round' 2 ratio)), "%"
    ])
 where
  ratio :: Double
  ratio = let ratio' = 100
                     * (fromIntegral sizeAfter - fromIntegral sizeBefore)
                     / fromIntegral sizeBefore
          in  if sizeBefore == 0 then 0 else ratio'

sayErrorF
  :: (Logging m, MonadTrans t)
  => Context
  -> T.Text
  -> UnifiedError
  -> t m ()
sayErrorF intro label theError =
  sayF intro (T.concat [label, "\t", T.pack (show theError)])
