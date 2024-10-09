module Data.FoldableWithRest
  ( FoldableWithRest (foldWithRest, foldMWithRest)
  )
  where

import Data.Kind (Constraint, Type)
import Data.Sequence (Seq (Empty, (:<|)))

{- |
The 'FoldableWithRest' typeclass provides a way to fold over a data structure
while keeping track of the rest of the data structure.
-}
type FoldableWithRest :: (Type -> Type) -> Constraint
class FoldableWithRest t where
  {- |
  Fold over a data structure while keeping track of the rest of the data
  structure.
  
  The function takes three arguments:
  - a function that takes the accumulator, the current item, and the rest of the
    data structure
  - an initial accumulator
  - the data structure to fold over
  -}
  foldWithRest
    :: (b -> a -> t a -> b)
    -> b
    -> t a
    -> b

  {- |
  Fold over a data structure while keeping track of the rest of the data
  structure in a monadic context.

  The function takes three arguments:
  - a function that takes the accumulator, the current item, and the rest of the
    data structure
  - an initial accumulator
  - the data structure to fold over
  -}
  foldMWithRest
    :: Monad m
    => (b -> a -> t a -> m b)
    -> b
    -> t a
    -> m b

instance FoldableWithRest Seq where
  foldWithRest :: (b -> a -> Seq a -> b) -> b -> Seq a -> b
  foldWithRest _func acc Empty = acc
  foldWithRest func acc (item :<| rest) = let result = func acc item rest
                                          in foldWithRest func result rest

  foldMWithRest :: Monad m => (b -> a -> Seq a -> m b) -> b -> Seq a -> m b
  foldMWithRest _func acc Empty = return acc
  foldMWithRest func acc (item :<| rest) = do
    result <- func acc item rest
    foldMWithRest func result rest

instance FoldableWithRest [] where
  foldWithRest :: (b -> a -> [a] -> b) -> b -> [a] -> b
  foldWithRest _func acc [] = acc
  foldWithRest func acc (item : rest) = let result = func acc item rest
                                        in foldWithRest func result rest

  foldMWithRest :: Monad m => (b -> a -> [a] -> m b) -> b -> [a] -> m b
  foldMWithRest _func acc [] = return acc
  foldMWithRest func acc (item : rest) = do
    result <- func acc item rest
    foldMWithRest func result rest
