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

import Fmt (commaizeF, fixedF, padLeftF, padRightF, (+|), (|+))

import System.IO (hFlush, stderr)

type Logging :: (Type -> Type) -> Constraint
class Monad m => Logging m where
  say :: Context -> T.Text -> m ()

-- This instance does not use the hPutStrLn function to ensure the output is
-- sent in a single write operation. This allows multiple threads to write to
-- the same handle without interleaving their output.
instance Logging IO where
  say :: Context -> T.Text -> IO ()
  say (Context intro) message = do
    TIO.hPutStr stderr (T.concat [intro, ": ", message, "\n"])
    hFlush stderr
  say NoContext message = do
    TIO.hPutStr stderr (T.concat [message, "\n"])
    hFlush stderr

instance Logging Identity where
  say :: Context -> T.Text -> Identity ()
  say _ _ = return ()

instance Logging (Writer [T.Text]) where
  say :: Context -> T.Text -> Writer [T.Text] ()
  say (Context intro) message = (void . tell . return) (T.concat [intro, ": ", message])
  say NoContext message = (void . tell . return) message

{-# INLINE sayF #-}
sayF :: (Logging m, MonadTrans t) => Context -> T.Text -> t m ()
sayF mIntro = lift . say mIntro

{-# INLINE sayComparisonF #-}
sayComparisonF
  :: (Logging m, MonadTrans t)
  => Context
  -> T.Text
  -> Int
  -> Int
  -> t m ()
sayComparisonF intro label sizeBefore sizeAfter = sayF
  intro
  (  padRightF 36 ' ' label
  |+ " "
  +| padLeftF 12 ' ' (commaizeF sizeBefore)
  |+ " -> "
  +| padLeftF 12 ' ' (commaizeF sizeAfter)
  |+ " ("
  +| padLeftF 8 ' ' (fixedF 2 ratio)
  |+ "%)"
  )
 where
  ratio :: Float
  ratio = let ratio' = 100
                     * (fromIntegral sizeAfter - fromIntegral sizeBefore)
                     / fromIntegral sizeBefore
          in  if sizeBefore == 0 then 0 else ratio'

{-# INLINE sayErrorF #-}
sayErrorF :: (Logging m, MonadTrans t) => Context -> T.Text -> UnifiedError -> t m ()
sayErrorF intro label theError =
  sayF intro (padRightF 32 ' ' label |+ " " +| T.pack (show theError) |+ "")
