{-|
Lightweight logging with contextual prefixes and helpers.

This module provides the `Logging` typeclass to abstract logging over various
monads (e.g. `IO`, `Writer`), and helpers to emit contextual messages. Contexts
from `Data.Context` can prefix messages with a label or progress indicator to
make logs more readable.

The `IO` instance writes to `stderr` in a single operation to avoid interleaved
output across threads. Pure instances (e.g. `Writer`) can accumulate messages
for later inspection.
 -}
module Data.Logging
  ( Logging(say)
  , sayF
  , sayComparisonF
  , sayErrorF
  ) where

import Control.Monad (void)
import Control.Monad.Identity (Identity)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Writer (Writer, tell)

import Data.Context (Context (Context, ContextProgress, NoContext))
import Data.Kind (Constraint, Type)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.UnifiedError (UnifiedError)

import System.IO (hFlush, stderr)

import Util.Number (round')

type Logging :: (Type -> Type) -> Constraint
{-|
A minimal logging interface for monads.

Implementations decide how to render and where to send messages. The `Context`
parameter allows callers to provide structured prefixes, such as labels or
progress indicators.

Minimal complete definition: `say`.
-}
class Monad m => Logging m where
  {-|
  Emit a message with a contextual prefix.

  Depending on the instance:

  - `IO`: writes to `stderr` and flushes immediately.
  - `Writer [Text]`: appends the message to the writer log.
  - `Identity` / `Either`: no-op.
  -}
  say :: Context -> T.Text -> m ()

{-|
Pad a `Text` value to a given width for aligned logging.

If the text is already wider than the given width, it is returned unchanged.
-}
padText :: T.Text -> Int -> T.Text
padText text width =
  let textLen = T.length text
  in  if textLen >= width
        then text
        else text <> T.replicate (width - textLen) " "

{-|
Pad a `Context` to a given width for aligned logging.
-}
padContext :: Context -> Int -> T.Text
padContext (Context intro) width = padText intro width
padContext (ContextProgress current total) width =
  let progressText = T.concat
        [ "["
        , T.pack (show current)
        , "/"
        , T.pack (show total)
        , "]"
        ]
  in padText progressText width
padContext NoContext width = padText "" width

{-|
Log to `stderr` with immediate flush to reduce interleaving.

This instance does not use the hPutStrLn function to ensure the output is sent
in a single write operation. This allows multiple threads to write to the same
handle without interleaving their output.
-}
instance Logging IO where
  say :: Context -> T.Text -> IO ()
  say context message = do
    TIO.hPutStr stderr (T.concat [padContext context 64, " ", message, "\n"])
    hFlush stderr

{- |
No-op logging in 'Either'.

This is useful when composing a pipeline that may short-circuit on errors
without producing log output.
-}
instance Logging (Either a) where
  say :: Context -> T.Text -> Either a ()
  say _anyContext _anyMessage = return ()

{- |
No-op logging in 'Identity'.

This is useful for pure code paths that must satisfy a 'Logging' constraint.
-}
instance Logging Identity where
  say :: Context -> T.Text -> Identity ()
  say _anyContext _anyMessage = return ()

{- |
Append formatted messages to the writer log.

Messages are recorded with the padded context followed by a tab and the
message body.
-}
instance Logging (Writer [T.Text]) where
  say :: Context -> T.Text -> Writer [T.Text] ()
  say context message =
    (void . tell . return) (T.concat [padContext context 64, "\t", message])

{-|
Lift a `say` call through a monad transformer.

Useful when logging from transformer stacks like `ReaderT` or `StateT`,
delegating the actual logging to the base monad instance.
-}
sayF :: (Logging m, MonadTrans t) => Context -> T.Text -> t m ()
sayF mIntro = lift . say mIntro

{-|
Emit a comparison line with label, sizes, and delta percentage.

Renders: label, size-before, size-after, and signed percentage change rounded to
two decimals. Useful for reporting transformation impacts (e.g. compression or
optimization results).
-}
sayComparisonF
  :: (Logging m, MonadTrans t)
  => Context
  -> T.Text
  -> Int
  -> Int
  -> t m ()
sayComparisonF intro label sizeBefore sizeAfter = sayF
  intro
  (T.concat
    [ padText label 42
    , padText (T.pack (show sizeBefore)) 12
    , padText (T.pack (show sizeAfter)) 12
    , padText ((if ratio > 0 then "+" else "") <> T.pack (show (round' 2 ratio)) <> "%") 8
    ])
 where
  ratio :: Double
  ratio = let ratio' = 100
                     * (fromIntegral sizeAfter - fromIntegral sizeBefore)
                     / fromIntegral sizeBefore
          in  if sizeBefore == 0 then 0 else ratio'

{- |
Emit an error line with a label and a 'UnifiedError'.

The output is rendered as: label, a tab, then the 'show'n error.
-}
sayErrorF
  :: (Logging m, MonadTrans t)
  => Context
  -> T.Text
  -> UnifiedError
  -> t m ()
sayErrorF intro label theError =
  sayF intro (T.concat [padText label 32, T.pack (show theError)])