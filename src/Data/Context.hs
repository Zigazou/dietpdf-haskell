module Data.Context
( Context(Context, NoContext)
, Contextual(ctx)
, noCtx
, ctx2
, ctx3
, ctx4
)
where

import Data.ByteString qualified as BS
import Data.Kind (Constraint, Type)
import Data.Text qualified as T

type Context :: Type
data Context = Context !T.Text
             | NoContext
             deriving stock (Eq, Show)

{-# INLINE noCtx #-}
noCtx :: Context
noCtx = NoContext

removeFirstAndLast :: T.Text -> T.Text
removeFirstAndLast text
  | T.length text <= 1 = T.empty
  | otherwise          = T.init (T.tail text)

instance Semigroup Context where
  (<>) :: Context -> Context -> Context
  NoContext <> NoContext               = NoContext
  NoContext <> Context message         = Context message
  Context message <> NoContext         = Context message
  Context message1 <> Context message2 = Context (message1 <> message2)

instance Monoid Context where
  mempty :: Context
  mempty = NoContext

type Contextual :: Type -> Constraint
class Contextual t where
  ctx :: t -> Context

instance Contextual T.Text where
  ctx :: T.Text -> Context
  ctx = Context

instance Contextual String where
  ctx :: String -> Context
  ctx = Context . T.pack

instance Contextual Int where
  ctx :: Int -> Context
  ctx = Context . T.pack . show

instance Contextual Integer where
  ctx :: Integer -> Context
  ctx = Context . T.pack . show

instance Contextual Double where
  ctx :: Double -> Context
  ctx = Context . T.pack . show

instance Contextual BS.ByteString where
  ctx :: BS.ByteString -> Context
  ctx = Context . removeFirstAndLast . T.pack . show

instance Contextual Char where
  ctx :: Char -> Context
  ctx = Context . T.singleton

ctx2 :: (Contextual a, Contextual b) => a -> b -> Context
ctx2 a b = ctx a <> ctx b

ctx3 :: (Contextual a, Contextual b, Contextual c) => a -> b -> c -> Context
ctx3 a b c = ctx a <> ctx b <> ctx c

ctx4 :: (Contextual a, Contextual b, Contextual c, Contextual d) => a -> b -> c -> d -> Context
ctx4 a b c d = ctx a <> ctx b <> ctx c <> ctx d
