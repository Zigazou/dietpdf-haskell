{-|
Unified error handling utilities for DietPDF.

This module collects a single error type ('UnifiedError') and provides
lightweight aliases and helpers for composing computations with error
propagation. Using a uniform error type allows convenient use of 'ExceptT' and
'Either' in place of nested conditionals.
-}
module Data.Fallible
  ( FallibleT
  , Fallible
  , tryF
  , ifFail
  ) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Class (MonadTrans (lift))

import Data.Kind (Type)
import Data.UnifiedError (UnifiedError)

{-|
Monad transformer carrying 'UnifiedError' failures.

Alias for @ExceptT UnifiedError@ over any base monad.
-}
type FallibleT :: (Type -> Type) -> Type -> Type
type FallibleT = ExceptT UnifiedError

{-|
Pure variant of 'FallibleT': either a successful value or a 'UnifiedError'.
-}
type Fallible :: Type -> Type
type Fallible = Either UnifiedError

{-|
Run a 'FallibleT' computation, capturing its result as 'Either'.

This lifts 'runExceptT' into the surrounding monad, returning 'Right' on
success and 'Left' with the 'UnifiedError' on failure.
-}
tryF :: Monad m => FallibleT m a -> FallibleT m (Fallible a)
tryF = lift . runExceptT

{-|
Execute a computation, handling failures with a provided recovery function.

When the computation succeeds, its result is returned. On failure, the handler
is invoked with the 'UnifiedError', and its result is returned.
-}
ifFail
  :: Monad m
  => FallibleT m a
  -> (UnifiedError -> FallibleT m a)
  -> FallibleT m a
ifFail computation inCaseOfFail = do
  tryF computation >>= \case
    Right result  -> return result
    Left  anError -> inCaseOfFail anError
