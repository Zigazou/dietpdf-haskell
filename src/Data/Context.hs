{-|
Build and combine human-readable processing contexts.

This module defines `Context`, a small utility type used to annotate logs,
errors, and messages with human-readable context such as a hierarchical path
(joined with " > ") and optional progress information (rendered as
"[current/total]").

It provides the `Contextual` typeclass to convert common values into `Context`s,
helper combinators (`ctx2`, `ctx3`, `ctx4`) to build composite contexts, and a
`compileContexts` function to fold a list of contexts into a single one with
sensible ordering.
-}
module Data.Context
( Context(Context, NoContext, ContextProgress)
, Contextual(ctx)
, noCtx
, ctx2
, compileContexts
)
where

import Data.ByteString (ByteString)
import Data.Kind (Constraint, Type)
import Data.List (sortBy)
import Data.Text qualified as T

{-|
A human-readable context value. Constructors:

* `Context` — a textual label segment. When combined, segments are joined with
  " > ".
* `ContextProgress` — progress information with /current/ and /total/. This is
  rendered as a prefix like "[current/total]" when combined with textual
  segments.
* `NoContext` — absence of context.
-}
type Context :: Type
data Context = Context !T.Text      -- ^ A textual context segment.
             | ContextProgress !Int -- ^ Current progress value.
                               !Int -- ^ Total progress value.
             | NoContext            -- ^ No context.
             deriving stock (Eq, Show)

-- | The empty context value.
noCtx :: Context
noCtx = NoContext

instance Ord Context where
  compare :: Context -> Context -> Ordering
  compare NoContext NoContext                               = EQ
  compare NoContext _context                                = LT
  compare _context NoContext                                = GT
  compare (Context _text1) (Context _text2)                 = EQ
  compare (Context _text) (ContextProgress _current _total) = LT
  compare (ContextProgress _current _total) (Context _text) = GT
  compare (ContextProgress _current1 _total1)
          (ContextProgress _current2 _total2) = EQ

{-|
Fold a list of contexts into a single `Context`.

Progress information (if present) is placed before textual segments so the final
message reads like "[current/total] foo > bar". Textual segments are joined with
" > ". `NoContext` values are ignored.

This function sorts and combines using the `Semigroup` instance to achieve a
stable, readable output.
-}
compileContexts :: [Context] -> Context
compileContexts = foldr (<>) NoContext . sortBy (flip compare) . reverse

{-|
Remove first and last characters from text (used to trim quotes from shown
ByteString values).
-}
removeFirstAndLast :: T.Text -> T.Text
removeFirstAndLast text
  | T.length text <= 1 = T.empty
  | otherwise          = T.init (T.tail text)

instance Semigroup Context where
  (<>) :: Context -> Context -> Context
  NoContext <> NoContext               = NoContext
  NoContext <> Context message         = Context message
  Context message <> NoContext         = Context message
  Context message1 <> Context message2 = Context (message1 <> " > " <> message2)
  NoContext <> ContextProgress current total = ContextProgress current total
  ContextProgress current total <> NoContext = ContextProgress current total
  ContextProgress current1 total1 <> ContextProgress _current2 _total2 = ContextProgress current1 total1
  Context message <> ContextProgress current total =
    Context ("[" <> T.pack (show current) <> "/" <> T.pack (show total) <> "] " <> message)
  ContextProgress current total <> Context message =
    Context ("[" <> T.pack (show current) <> "/" <> T.pack (show total) <> "] " <> message)

instance Monoid Context where
  mempty :: Context
  mempty = NoContext

{-|
Types that can be converted to a `Context`.

Minimal complete definition: `ctx`.
-}
type Contextual :: Type -> Constraint
class Contextual t where
  ctx :: t -> Context

-- | Convert `Text` to a `Context` segment.
instance Contextual T.Text where
  ctx :: T.Text -> Context
  ctx = Context

-- | Convert `String` to a `Context` segment.
instance Contextual String where
  ctx :: String -> Context
  ctx = Context . T.pack

-- | Convert `Int` to a `Context` segment.
instance Contextual Int where
  ctx :: Int -> Context
  ctx = Context . T.pack . show

-- | Convert `Integer` to a `Context` segment.
instance Contextual Integer where
  ctx :: Integer -> Context
  ctx = Context . T.pack . show

-- | Convert `Double` to a `Context` segment.
instance Contextual Double where
  ctx :: Double -> Context
  ctx = Context . T.pack . show

-- | Convert a pair of `Double`s to a dotted `Context` segment.
instance Contextual (Double, Double) where
  ctx :: (Double, Double) -> Context
  ctx (a, b) = Context $ T.pack (show a) <> "." <> T.pack (show b)

-- | Convert a pair of `Int`s to a dotted `Context` segment.
instance Contextual (Int, Int) where
  ctx :: (Int, Int) -> Context
  ctx (a, b) = Context $ T.pack (show a) <> "." <> T.pack (show b)

{-|
Convert a `ByteString` to a `Context` segment, trimming quotes from its shown
form.
-}
instance Contextual ByteString where
  ctx :: ByteString -> Context
  ctx = Context . removeFirstAndLast . T.pack . show

-- | Convert a `Char` to a single-character `Context` segment.
instance Contextual Char where
  ctx :: Char -> Context
  ctx = Context . T.singleton

{-|
Combine two values into a composite `Context`.

The values are converted via `ctx` and concatenated using the `Semigroup`
instance.
-}
ctx2 :: (Contextual a, Contextual b) => a -> b -> Context
ctx2 a b = ctx a <> ctx b
