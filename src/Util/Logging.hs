{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module Util.Logging
  ( Logging(say)
  , sayF
  , sayComparisonF
  , sayErrorF
  ) where

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Control.Monad.Identity         ( Identity )
import           Control.Monad.Writer           ( Writer
                                                , tell
                                                )
import           Control.Monad                  ( void )
import           Control.Monad.Trans.Class      ( lift
                                                , MonadTrans
                                                )
import           Fmt                            ( padLeftF
                                                , (+|)
                                                , (|+)
                                                , commaizeF
                                                , padRightF
                                                , fixedF
                                                )
import           Util.UnifiedError              ( UnifiedError )
import           System.IO                      ( stderr )

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
  +| padRightF 32 ' ' label
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
