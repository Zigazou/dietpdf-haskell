-- | This modules contains functions to help dealing with Text strings.
module Util.Text
  ( txtNumberVersion
  ) where

import qualified Data.Text as T

txtNumberVersion :: Int -> Int -> T.Text
txtNumberVersion number version =
  T.pack (show number) <> "." <> T.pack (show version)
