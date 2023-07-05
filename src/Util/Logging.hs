{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module Util.Logging
  ( Logging(say, action)
  , sayF
  , actionF
  ) where

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Control.Monad.Identity         ( Identity )
import           Control.Monad.Writer           ( Writer
                                                , tell
                                                )
import           Control.Monad                  ( void )
import           Control.Monad.Trans.Class      ( lift, MonadTrans )

class Monad m => Logging m where
  say :: T.Text -> m ()

  action :: T.Text -> a -> m a
  action t a = say t >> return a

instance Logging IO where
  say :: T.Text -> IO ()
  say = TIO.putStrLn

instance Logging Identity where
  say :: T.Text -> Identity ()
  say _ = return ()

instance Logging (Writer [T.Text]) where
  say :: T.Text -> Writer [T.Text] ()
  say = void . tell . return

sayF :: (Logging m, MonadTrans t) => T.Text -> t m ()
sayF = lift . say

actionF :: (Logging m, MonadTrans t, Monad (t m)) => T.Text -> a -> t m a
actionF t a = sayF t >> return a
