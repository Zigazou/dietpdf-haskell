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

type Display :: Type -> Constraint
class Display a where
  disp :: a -> FallibleT IO ()

instance Display String where
  disp :: String -> FallibleT IO ()
  disp = liftIO . putStrLn

instance Display [String] where
  disp :: [String] -> FallibleT IO ()
  disp = liftIO . putStrLn . intercalate "\t"

instance Display [T.Text] where
  disp :: [T.Text] -> FallibleT IO ()
  disp = liftIO . TIO.putStrLn . T.intercalate "\t"

instance Display [TL.Text] where
  disp :: [TL.Text] -> FallibleT IO ()
  disp = liftIO . TIO.putStrLn . TL.toStrict . TL.intercalate "\t"

instance Display [ByteString] where
  disp :: [ByteString] -> FallibleT IO ()
  disp = liftIO . putStrLn . BSU.toString . BS.intercalate "\t"
