{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module TradingGame.Concurrent (Concurrent(..), withWorkers, runConcurrent) where

import Control.Concurrent.Async (link, mapConcurrently_, withAsync)
import Control.Effect (Eff, Effect, IOE, (:<), handle, locally, send)
import qualified Control.Effect.Internal as Internal

-- Workers live only for the duration of the body. A worker failure aborts the
-- body; returning or throwing from the body cancels and joins every worker.
data Concurrent :: Effect where
  WithWorkers :: [m ()] -> m a -> Concurrent m a

withWorkers :: Concurrent :< effs => [Eff effs ()] -> Eff effs a -> Eff effs a
withWorkers workers body = send (WithWorkers workers body)

runConcurrent :: IOE :< effs => Eff (Concurrent ': effs) a -> Eff effs a
runConcurrent = handle pure $ \(WithWorkers workers body) ->
  locally (scopedWorkers workers body)

-- The pinned eff revision has no public IO unlifting API. Keep its internal
-- dependency here: each action borrows the current handler environment, and
-- withAsync ensures it cannot outlive that environment. Install a prompt in
-- each thread so delimited control inside a worker (including stepPlayer) works.
-- Outer handlers are shared and must be thread-safe. Control operations may
-- target handlers installed inside an action, but cannot capture/abort across
-- this IO boundary; fail explicitly instead of letting an RTS prompt escape.
scopedWorkers :: forall effs a. [Eff effs ()] -> Eff effs a -> Eff effs a
scopedWorkers workers body = Internal.Eff $ \registers -> do
  let lower :: Eff effs b -> IO b
      lower action = Internal.promptVM
        (Internal.unEff action registers)
        pure
        (\_ _ -> boundaryError)
        (\_ -> boundaryError)
      boundaryError :: IO b
      boundaryError = ioError (userError
        "runConcurrent: nonlocal effect control crossed a worker scope; install its handler inside the action")
  result <- withAsync (mapConcurrently_ lower workers) $ \running -> do
    link running
    lower body
  pure (registers, result)
