{-|
Simple display utilities for tab-separated output.

This module defines a `Display` typeclass with a single method `disp` to print
values to standard output. Instances are provided for common string and text
collections, rendering list elements separated by tabs.
-}
module Util.Display
  ( disp
  ) where

import Control.Monad.IO.Class (liftIO)

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.UTF8 qualified as BSU
import Data.Fallible (FallibleT)
import Data.Kind (Constraint, Type)
import Data.List (intercalate)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Lazy qualified as TL

{-|
Types that can be displayed via `disp`.

Minimal complete definition: `disp`.
-}
type Display :: Type -> Constraint
class Display a where
  disp :: a -> FallibleT IO ()

{-|
Display a `String` line to standard output.
-}
instance Display String where
  disp :: String -> FallibleT IO ()
  disp = liftIO . putStrLn

{-|
Display a list of `String`s separated by tab characters.
-}
instance Display [String] where
  disp :: [String] -> FallibleT IO ()
  disp = liftIO . putStrLn . intercalate "\t"

{-|
Display a list of strict `Text` values separated by tabs.
-}
instance Display [T.Text] where
  disp :: [T.Text] -> FallibleT IO ()
  disp = liftIO . TIO.putStrLn . T.intercalate "\t"

{-|
Display a list of lazy `Text` values separated by tabs.
-}
instance Display [TL.Text] where
  disp :: [TL.Text] -> FallibleT IO ()
  disp = liftIO . TIO.putStrLn . TL.toStrict . TL.intercalate "\t"

{-|
Display a list of `ByteString`s separated by tabs, decoding as UTF-8.
-}
instance Display [ByteString] where
  disp :: [ByteString] -> FallibleT IO ()
  disp = liftIO . putStrLn . BSU.toString . BS.intercalate "\t"
