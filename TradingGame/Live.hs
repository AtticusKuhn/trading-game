{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module TradingGame.Live where

import Control.Concurrent.Async (link, mapConcurrently_, withAsync)
import Control.Concurrent.STM
import Control.DeepSeq (rnf)
import Control.Exception (evaluate, finally)
import Control.Monad (when)
import Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime, diffUTCTime, getCurrentTime)
import GHC.Clock (getMonotonicTimeNSec)
import Numeric.Natural (Natural)
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

-- Host-only instrumentation, in exchange processing order. A parked settlement
-- produces another RequestHandled when its final answer is calculated at close.
data LiveEvent where
  RequestHandled :: UTCTime -> PlayerId -> TradingGame m a -> Decision a -> LiveEvent
  ExchangeClosed :: UTCTime -> LiveEvent

data LiveConfig = LiveConfig
  { liveDuration :: NominalDiffTime
  , liveQueueCapacity :: Natural
  -- Runs synchronously in the exchange thread. Keep it short; exceptions abort
  -- the run. The trace is private to the host, including private-number replies.
  , onLiveEvent :: LiveEvent -> IO ()
  }

defaultLiveConfig :: LiveConfig
defaultLiveConfig = LiveConfig 3600 256 (const (pure ()))

data LiveFailure = LiveStopped deriving (Eq, Show)

-- Continuations remain in player workers; only typed requests cross the queue.
data Request where
  Request :: PlayerId -> TradingGame m a -> TMVar (Decision a) -> Request

data LiveRuntime = LiveRuntime
  { requestQueue :: TBQueue Request
  , runtimeStopped :: TVar Bool
  }

newLiveRuntime :: Natural -> IO LiveRuntime
newLiveRuntime capacity = do
  when (capacity == 0) $ ioError (userError "liveQueueCapacity must be positive")
  LiveRuntime <$> newTBQueueIO capacity <*> newTVarIO False

-- Admission and reply waiting MUST be separate transactions. The stopped flag
-- wakes both a producer blocked on a full queue and a caller waiting for a reply.
requestLive
  :: LiveRuntime -> PlayerId -> TradingGame m a
  -> IO (Either LiveFailure (Decision a))
requestLive runtime pid request = do
  reply <- newEmptyTMVarIO
  admitted <- atomically $ do
    stopped <- readTVar (runtimeStopped runtime)
    if stopped then pure False else do
      writeTBQueue (requestQueue runtime) (Request pid request reply)
      pure True
  if not admitted then pure (Left LiveStopped) else atomically $
    (Right <$> readTMVar reply) `orElse`
    (awaitStop runtime >> pure (Left LiveStopped))

awaitStop :: LiveRuntime -> STM ()
awaitStop runtime = readTVar (runtimeStopped runtime) >>= check

data PendingSettlement = PendingSettlement PlayerId (TMVar (Decision Settlement))

-- A single owner serializes the exchange; STM coordinates transport, not matching.
-- Each request is timestamped immediately before applying the shared rules.
-- The independent alarm closes an idle exchange; checking time on every loop
-- also prevents a busy queue from starving closure.
runExchange :: LiveClock -> (LiveEvent -> IO ()) -> LiveRuntime -> Engine -> IO Engine
runExchange clock trace runtime initial =
  (do
    alarm <- clockAlarm clock (closesAt (engineInfo initial))
    loop alarm initial [])
  `finally` atomically (writeTVar (runtimeStopped runtime) True)
  where
    loop alarm engine parked = do
      now <- clockNow clock
      current <- evaluate (advanceTo now engine)
      case enginePhase current of
        Resolved _ -> do
          trace (ExchangeClosed now)
          mapM_ (settle now current) (reverse parked)
          pure current
        Trading -> do
          next <- atomically $
            (alarm >> pure Nothing) `orElse`
            (Just <$> readTBQueue (requestQueue runtime))
          case next of
            Nothing -> loop alarm current parked
            Just (Request pid request reply) -> do
              handledAt <- clockNow clock
              let (updated, decision) = handleRequest handledAt pid request current
              _ <- evaluate updated
              trace (RequestHandled handledAt pid request decision)
              case decision of
                WhenResolved -> loop alarm updated (PendingSettlement pid reply : parked)
                _ -> do
                  atomically (putTMVar reply decision)
                  loop alarm updated parked

    settle now engine (PendingSettlement pid reply) = do
      let (_, decision) = handleRequest now pid AwaitSettlement engine
      trace (RequestHandled now pid AwaitSettlement decision)
      atomically (putTMVar reply decision)

-- Force request arguments in the originating worker so player computations do
-- not travel to the exchange as unevaluated order prices, quantities, or waits.
forceRequest :: TradingGame m a -> ()
forceRequest request = case request of
  SubmitOrder order ->
    let Price price = limitPrice order
    in orderSide order `seq` rnf (price, orderQuantity order)
  Wait seconds -> rnf seconds
  WaitUntil target -> rnf target
  _ -> ()

runLivePlayer :: LiveClock -> UTCTime -> LiveRuntime -> Player -> IO ()
runLivePlayer clock close runtime (pid, program, _) = loop program
  where
    loop current = do
      step <- evaluate (stepPlayer current)
      case step of
        Finished -> pure ()
        Requested request resume -> do
          _ <- evaluate (forceRequest request)
          answer <- requestLive runtime pid request
          case answer of
            Left LiveStopped -> pure ()
            Right (Reply value) -> loop (resume value)
            Right (ResumeAt target)
              -- Live execution deliberately drops activity at/after closure.
              | target >= close -> atomically (awaitStop runtime)
              | otherwise -> do
                  alarm <- clockAlarm clock target
                  awake <- atomically $
                    (awaitStop runtime >> pure False) `orElse` (alarm >> pure True)
                  when awake (loop (resume ()))
            Right WhenResolved -> error "runLivePlayer: unresolved settlement reply"

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
  runtime <- newLiveRuntime (liveQueueCapacity config)
  start <- clockNow clock
  let initial = newEngine start (liveDuration config)
        [(pid, toInteger secret) | (pid, _, secret) <- players]
      worker = runLivePlayer clock (closesAt (engineInfo initial)) runtime
  withAsync (mapConcurrently_ worker players) $ \workers -> do
    link workers
    runExchange clock (onLiveEvent config) runtime initial
