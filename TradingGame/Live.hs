{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module TradingGame.Live where

import Control.Effect (Eff, IOE, (:<), interpret, liftIO)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (evaluate)
import Control.Monad (when)
import Data.Set (Set)
import Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime, diffUTCTime, getCurrentTime)
import GHC.Clock (getMonotonicTimeNSec)
import System.Random (newStdGen)
import TradingGame.Concurrent
import TradingGame.Core
import TradingGame.Player
import TradingGame.Session
import TradingGame.Reveals

-- The alarm action must become ready once clockNow >= its target. Both parts
-- must use the same nondecreasing clock. Tests can implement both with one TVar.
data LiveClock = LiveClock
  { clockNow :: IO UTCTime
  , clockAlarm :: UTCTime -> IO (STM ())
  }

-- UTC is anchored once; elapsed game time is immune to wall-clock adjustments.
newLiveClock :: IO LiveClock
newLiveClock = do
  epoch <- getCurrentTime
  origin <- getMonotonicTimeNSec
  let now = do
        ticks <- getMonotonicTimeNSec
        pure $ addUTCTime
          (fromRational (toRational (toInteger ticks - toInteger origin) / 1000000000)) epoch
      alarm target = do
        current <- now
        let micros = max 0 (ceiling (diffUTCTime target current * 1000000) :: Integer)
        if micros == 0 then pure (pure ()) else do
          when (micros > toInteger (maxBound :: Int)) $
            ioError (userError "clockAlarm: deadline exceeds the timer range")
          fired <- registerDelay (fromInteger micros)
          pure (readTVar fired >>= check)
  pure (LiveClock now alarm)

-- Host-only instrumentation, serialized with engine updates. Deferred settlement
-- requests are recorded once as WhenResolved; all waiters share the final engine.
data LiveEvent where
  RequestHandled :: UTCTime -> PlayerId -> TradingGame m a -> Decision a -> LiveEvent
  ExchangeClosed :: UTCTime -> LiveEvent

data LiveConfig = LiveConfig
  { liveDuration :: NominalDiffTime
  , liveInstruments :: Set Instrument
  , liveRevealPlan :: Maybe [(UTCTime, PlayerId)]
  -- Runs under the engine lock. Keep it short; exceptions abort the run.
  -- Traces are private to the host, including private-number replies.
  , onLiveEvent :: LiveEvent -> IO ()
  }

defaultLiveConfig :: LiveConfig
defaultLiveConfig = LiveConfig 3600 allInstruments Nothing (const (pure ()))

data LiveRuntime = LiveRuntime
  { runtimeClock :: LiveClock
  , runtimeTrace :: LiveEvent -> IO ()
  , runtimeEngine :: MVar Engine
  , runtimeSnapshot :: TVar ExchangeState
  , runtimeFinal :: TMVar Engine
  }

newLiveRuntime :: LiveClock -> (LiveEvent -> IO ()) -> Engine -> IO LiveRuntime
newLiveRuntime clock trace initial =
  LiveRuntime clock trace <$> newMVar initial
    <*> newTVarIO (exchangeSnapshot (opensAt (engineInfo initial)) initial) <*> newEmptyTMVarIO

-- Sample time only after acquiring the lock. Masking keeps final publication
-- and the engine commit together; exceptions restore the previous engine.
modifyLiveEngine :: LiveRuntime -> (UTCTime -> Engine -> IO (Engine, a)) -> IO a
modifyLiveEngine runtime action = modifyMVarMasked (runtimeEngine runtime) $ \engine -> do
  now <- clockNow (runtimeClock runtime)
  (updated, result) <- action now engine
  _ <- evaluate updated
  case (enginePhase engine, enginePhase updated) of
    (Trading, Resolved _) -> do
      runtimeTrace runtime (ExchangeClosed now)
      atomically (putTMVar (runtimeFinal runtime) updated)
    _ -> pure ()
  atomically $ writeTVar (runtimeSnapshot runtime) (exchangeSnapshot now updated)
  pure (updated, result)

-- Workers apply the shared rules directly. Waiting happens outside the lock;
-- requests processed at/after the deadline see the resolved engine.
requestLive :: LiveRuntime -> PlayerId -> TradingGame m a -> IO (Decision a)
requestLive runtime pid request = do
  modifyLiveEngine runtime $ \now engine -> do
    let (updated, decision) = handleRequest now pid request engine
    _ <- evaluate updated
    runtimeTrace runtime (RequestHandled now pid request decision)
    pure (updated, decision)

-- Publish reveals and close even when every player has finished or is waiting.
-- Requests also advance the engine, so scheduler latency cannot hide due events
-- from an active caller or allow trading past the deadline.
runExchange :: LiveRuntime -> IO Engine
runExchange runtime = do
  current <- modifyLiveEngine runtime $ \now engine ->
    let updated = advanceTo now engine in pure (updated, updated)
  case enginePhase current of
    Resolved _ -> pure current
    Trading -> do
      let target = case pendingReveals current of
            (time, _):_ -> time
            [] -> closesAt (engineInfo current)
      clockAlarm (runtimeClock runtime) target >>= atomically
      runExchange runtime

-- Bind the entire inner computation to the identity selected by the outer
-- session. Rejected sessions do not execute any part of the computation.
-- Waits run outside the engine lock. Unlike a supervised player worker, this
-- interpreter also supports arbitrary return values and queries after closure.
runWithCurrentPlayer
  :: (PlayerSession :< effs, IOE :< effs)
  => LiveRuntime
  -> Eff (TradingGame ': effs) a
  -> Eff effs (Either SessionError a)
runWithCurrentPlayer runtime action = do
  current <- getCurrentPlayer
  case current of
    Nothing -> pure (Left NotLoggedIn)
    Just pid -> do
      roster <- liftIO (players <$> readMVar (runtimeEngine runtime))
      if pid `notElem` map playerID roster
        then pure (Left (UnknownPlayerId pid))
        else Right <$> interpret (liftIO . handleLiveRequest runtime pid) action

-- Waiting happens outside the engine lock. The snapshot comparison and STM
-- subscription are atomic, so changes between requests cannot be lost.
handleLiveRequest :: LiveRuntime -> PlayerId -> TradingGame m a -> IO a
handleLiveRequest runtime pid request = do
  decision <- requestLive runtime pid request
  case decision of
    Reply value -> pure value
    ResumeAt target -> clockAlarm (runtimeClock runtime) target >>= atomically
    WhenResolved -> settlementFor pid <$> runExchange runtime
    WhenRevealed target value -> do
      clockAlarm (runtimeClock runtime) target >>= atomically
      modifyLiveEngine runtime $ \now engine -> pure (advanceTo now engine, Just value)
    WhenExchangeChanges previous -> do
      atomically (readTVar (runtimeSnapshot runtime) >>= check . exchangeChanged previous)
      handleLiveRequest runtime pid GetExchangeState

-- Each request is interpreted directly, so child threads never capture across
-- the IO boundary in runConcurrent. The enclosing live runner owns cancellation
-- at closure; standalone callers can continue inspecting the resolved game.
runLivePlayer :: IOE :< effs => LiveRuntime -> PlayerProgram effs -> Eff effs ()
runLivePlayer runtime (player, program) = runConcurrent $
  interpret (liftIO . handleLiveRequest runtime (playerID player)) program

runLive :: (IOE :< effs, Concurrent :< effs) => [PlayerProgram effs] -> Eff effs [Settlement]
runLive = runLiveFor 3600

runLiveFor :: (IOE :< effs, Concurrent :< effs) => NominalDiffTime -> [PlayerProgram effs] -> Eff effs [Settlement]
runLiveFor duration programs = do
  clock <- liftIO newLiveClock
  runLiveWith clock defaultLiveConfig { liveDuration = duration } programs

-- Like runTradingGame, results follow input order. Ends at closure even when
-- players finish early or wait beyond it. There is no guarantee that a player's
-- continuation after awaitSettlement runs. Workers are trusted, interruptible
-- Haskell computations; this is not isolation for untrusted/noninterruptible code.
-- Any worker/exchange exception aborts the run and cleans up the other workers.
runLiveWith :: (IOE :< effs, Concurrent :< effs) => LiveClock -> LiveConfig -> [PlayerProgram effs] -> Eff effs [Settlement]
runLiveWith clock config programs = engineSettlements <$> runLiveEngineWith clock config programs

runLiveEngineWith :: (IOE :< effs, Concurrent :< effs) => LiveClock -> LiveConfig -> [PlayerProgram effs] -> Eff effs Engine
runLiveEngineWith clock config programs = do
  start <- liftIO (clockNow clock)
  seed <- liftIO newStdGen
  let roster = map fst programs
      plan = maybe (sampleRevealTargets seed roster
        (defaultRevealTimes start (addUTCTime (liveDuration config) start) (length roster))) id
        (liveRevealPlan config)
      initial = newEngineWithReveals (liveInstruments config) start (liveDuration config) roster plan
  runtime <- liftIO (newLiveRuntime clock (onLiveEvent config) initial)
  let worker = runLivePlayer runtime
  withWorkers (map worker programs) (liftIO (runExchange runtime))
