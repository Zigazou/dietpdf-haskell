{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module Util.Step
  ( StepM(step, action)
  ) where

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Control.Monad.Identity         ( Identity )
import           Control.Monad.Writer           ( Writer
                                                , tell
                                                )
import           Control.Monad                  ( void )

class Monad m => StepM m where
  step :: T.Text -> m ()

  action :: T.Text -> a -> m a
  action t a = step t >> return a

instance StepM IO where
  step :: T.Text -> IO ()
  step = TIO.putStrLn

instance StepM Identity where
  step :: T.Text -> Identity ()
  step _ = return ()

instance StepM (Writer [T.Text]) where
  step :: T.Text -> Writer [T.Text] ()
  step = void . tell . return
