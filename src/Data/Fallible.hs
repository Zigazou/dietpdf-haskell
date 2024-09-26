{-|
This module groups all the errors that DietPDF may generate.

Having one type for all errors means the Either monad can be used to avoid
long if then else if then else.
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

type FallibleT :: (Type -> Type) -> Type -> Type
type FallibleT = ExceptT UnifiedError

type Fallible :: Type -> Type
type Fallible = Either UnifiedError

{-# INLINE tryF #-}
tryF :: Monad m => FallibleT m a -> FallibleT m (Fallible a)
tryF = lift . runExceptT

{-# INLINE ifFail #-}
ifFail
  :: Monad m
  => FallibleT m a
  -> (UnifiedError -> FallibleT m a)
  -> FallibleT m a
ifFail computation inCaseOfFail = do
  tryF computation >>= \case
    Right result  -> return result
    Left  anError -> inCaseOfFail anError
