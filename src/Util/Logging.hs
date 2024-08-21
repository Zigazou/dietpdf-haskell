module Util.Logging
  ( Logging(say)
  , sayF
  , sayComparisonF
  , sayErrorF
  ) where

import Control.Monad (void)
import Control.Monad.Identity (Identity)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Writer (Writer, tell)

import Data.Kind (Constraint, Type)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

import Fmt (commaizeF, fixedF, padLeftF, padRightF, (+|), (|+))

import System.IO (stderr)

import Util.UnifiedError (UnifiedError)

type Logging :: (Type -> Type) -> Constraint
class Monad m => Logging m where
  say :: T.Text -> m ()

instance Logging IO where
  say :: T.Text -> IO ()
  say = TIO.hPutStrLn stderr

instance Logging Identity where
  say :: T.Text -> Identity ()
  say _ = return ()

instance Logging (Writer [T.Text]) where
  say :: T.Text -> Writer [T.Text] ()
  say = void . tell . return

sayF :: (Logging m, MonadTrans t) => T.Text -> t m ()
sayF = lift . say

sayComparisonF :: (Logging m, MonadTrans t) => T.Text -> Int -> Int -> t m ()
sayComparisonF label sizeBefore sizeAfter = sayF
  (  "  - "
  +| padRightF 36 ' ' label
  |+ " "
  +| padLeftF 12 ' ' (commaizeF sizeBefore)
  |+ "/"
  +| padLeftF 12 ' ' (commaizeF sizeAfter)
  |+ " ("
  +| padLeftF 8 ' ' (fixedF 2 ratio)
  |+ "%)"
  )
 where
  ratio :: Float
  ratio =
    100
      * (fromIntegral sizeAfter - fromIntegral sizeBefore)
      / fromIntegral sizeBefore

sayErrorF :: (Logging m, MonadTrans t) => T.Text -> UnifiedError -> t m ()
sayErrorF label theError =
  sayF ("  - " +| padRightF 32 ' ' label |+ " " +| T.pack (show theError) |+ "")
