{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module TradingGame.Live where

import Control.Concurrent.Async (link, mapConcurrently_, withAsync)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Control.Monad (void, when)
import Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime, diffUTCTime, getCurrentTime)
import GHC.Clock (getMonotonicTimeNSec)
import TradingGame.Core
import TradingGame.Player

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
  -- Runs under the engine lock. Keep it short; exceptions abort the run.
  -- Traces are private to the host, including private-number replies.
  , onLiveEvent :: LiveEvent -> IO ()
  }

defaultLiveConfig :: LiveConfig
defaultLiveConfig = LiveConfig 3600 (const (pure ()))

data LiveRuntime = LiveRuntime
  { runtimeClock :: LiveClock
  , runtimeTrace :: LiveEvent -> IO ()
  , runtimeEngine :: MVar Engine
  , runtimeFinal :: TMVar Engine
  }

newLiveRuntime :: LiveClock -> (LiveEvent -> IO ()) -> Engine -> IO LiveRuntime
newLiveRuntime clock trace initial =
  LiveRuntime clock trace <$> newMVar initial <*> newEmptyTMVarIO

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
  pure (updated, result)

-- Workers apply the shared rules directly. Waiting happens outside the lock;
-- requests processed at/after the deadline see the resolved engine.
requestLive :: LiveRuntime -> PlayerId -> TradingGame m a -> IO (Decision a)
requestLive runtime pid request = do
  _ <- evaluate (forceRequest request)
  modifyLiveEngine runtime $ \now engine -> do
    let (updated, decision) = handleRequest now pid request engine
    _ <- evaluate updated
    runtimeTrace runtime (RequestHandled now pid request decision)
    pure (updated, decision)

-- Close even when every player has finished or is waiting. Requests also close
-- the engine at the deadline, so active players cannot keep trading past it.
runExchange :: LiveRuntime -> IO Engine
runExchange runtime = do
  initial <- readMVar (runtimeEngine runtime)
  clockAlarm (runtimeClock runtime) (closesAt (engineInfo initial)) >>= atomically
  modifyLiveEngine runtime $ \now engine ->
    let final = advanceTo now engine in pure (final, final)

-- Force request arguments in the originating worker so player computations do
-- not hold the engine lock while evaluating order prices, quantities, or waits.
forceRequest :: TradingGame m a -> ()
forceRequest request = case request of
  SubmitOrder order ->
    let Price price = limitPrice order
    in orderSide order `seq` rnf (price, orderQuantity order)
  Wait seconds -> rnf seconds
  WaitUntil target -> rnf target
  _ -> ()

runLivePlayer :: UTCTime -> LiveRuntime -> Player -> IO ()
runLivePlayer close runtime (pid, program, _) = loop program
  where
    finished = readTMVar (runtimeFinal runtime)
    loop current = do
      step <- evaluate (stepPlayer current)
      case step of
        Finished -> pure ()
        Requested request resume -> do
          answer <- requestLive runtime pid request
          case answer of
            Reply value -> loop (resume value)
            ResumeAt target
              | target >= close -> void (atomically finished)
              | otherwise -> do
                  alarm <- clockAlarm (runtimeClock runtime) target
                  awake <- atomically $
                    (finished >> pure False) `orElse` (alarm >> pure True)
                  when awake (loop (resume ()))
            WhenResolved -> do
              final <- atomically finished
              loop (resume (settlementFor (engineTotal final) pid (engineBook final)))

runLive :: [Player] -> IO [Settlement]
runLive = runLiveFor 3600

runLiveFor :: NominalDiffTime -> [Player] -> IO [Settlement]
runLiveFor duration players = do
  clock <- newLiveClock
  runLiveWith clock defaultLiveConfig { liveDuration = duration } players

-- Like runTradingGame, results follow input order. Ends at closure even when
-- players finish early or wait beyond it. There is no guarantee that a player's
-- continuation after awaitSettlement runs. Workers are trusted, interruptible
-- Haskell computations; this is not isolation for untrusted/noninterruptible code.
-- Any worker/exchange exception aborts the run and cleans up the other workers.
runLiveWith :: LiveClock -> LiveConfig -> [Player] -> IO [Settlement]
runLiveWith clock config players = engineSettlements <$> runLiveEngineWith clock config players

runLiveEngineWith :: LiveClock -> LiveConfig -> [Player] -> IO Engine
runLiveEngineWith clock config players = do
  start <- clockNow clock
  let initial = newEngine start (liveDuration config)
        [(pid, toInteger secret) | (pid, _, secret) <- players]
  runtime <- newLiveRuntime clock (onLiveEvent config) initial
  let worker = runLivePlayer (closesAt (engineInfo initial)) runtime
  withAsync (mapConcurrently_ worker players) $ \workers -> do
    link workers
    runExchange runtime
