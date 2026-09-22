{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module TradingGame.Player
  (PlayerProgram, Scheduling(..), Task, PlayerStep(..), preparePlayer, stepPlayer) where

import Control.Effect (Eff, Effect, control0, handle, interpret, lift, liftH, send)
import qualified Control.Effect.Internal as Internal
import Control.Monad (forM_, when)
import TradingGame.Concurrent
import TradingGame.Core

type PlayerProgram effs = (Player, Eff (TradingGame ': Concurrent ': effs) ())

-- First-order scheduling instructions let continuation capture retain all local
-- handlers, even when a worker is created inside another worker's scope.
-- Fork resumes the same continuation twice: once in the child (ending at Stop),
-- once in the parent. No IO or borrowed handler environments are needed.
data Scheduling :: Effect where
  TradingRequest :: TradingGame n a -> Scheduling m a
  EnterScope :: Scheduling m ()
  ForkTask :: Scheduling m Bool
  ExitScope :: Scheduling m ()
  StopTask :: Scheduling m a

type Task effs = Eff (Scheduling ': effs) ()

data PlayerStep effs where
  Finished :: PlayerStep effs
  Requested :: Scheduling m a -> (a -> Task effs) -> PlayerStep effs

preparePlayer :: Eff (TradingGame ': Concurrent ': effs) () -> Task effs
-- Handle has no Monad instance in the pinned eff. Its internal context lets
-- us sequence liftH operations with local worker actions without changing the
-- continuation machinery or introducing an IO/thread boundary.
preparePlayer program =
  handle pure (\(WithWorkers workers body) -> Internal.Handle $ \context -> do
    Internal.runHandle (liftH (schedule EnterScope)) context
    forM_ workers $ \worker -> do
      child <- Internal.runHandle (liftH (schedule ForkTask)) context
      when child $ do
        worker
        Internal.runHandle (liftH (schedule StopTask)) context
    result <- body
    Internal.runHandle (liftH (schedule ExitScope)) context
    pure result) $
    interpret (send . TradingRequest) (lift program)

stepPlayer :: Task effs -> Eff effs (PlayerStep effs)
stepPlayer = handle (\() -> pure Finished)
  (\request -> control0 $ \resume -> pure (Requested request resume))

schedule :: Scheduling (Eff (Concurrent ': Scheduling ': effs)) a -> Eff (Concurrent ': Scheduling ': effs) a
schedule = send
