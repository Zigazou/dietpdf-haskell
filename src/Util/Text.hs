{-|
Small helpers for building 'Text' values.

This module contains small helpers around 'Data.Text' used throughout the
project.
-}
module Util.Text
  ( txtNumberVersion
  ) where

import Data.Text qualified as T

{-|
Render a two-part version number as dotted text.

This is typically used for PDF versions (e.g. @1.4@), where the first argument
is the major number and the second is the minor number.
-}
txtNumberVersion :: Int -> Int -> T.Text
txtNumberVersion number version =
  T.pack (show number) <> "." <> T.pack (show version)
