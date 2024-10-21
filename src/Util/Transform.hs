module Util.Transform
  ( untilNoChange
  ) where

untilNoChange :: Eq a => (a -> a) -> a -> a
untilNoChange transform original
  | original == transformed = transformed
  | otherwise               = untilNoChange transform transformed
  where transformed = transform original
