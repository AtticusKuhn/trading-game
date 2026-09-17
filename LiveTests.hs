{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LiveTests (liveProperties, runLiveTests) where

import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM
import Control.Exception (SomeException, displayException, throwIO, try)
import Control.Monad (foldM, forM, unless, void)
import Data.Time.Clock (UTCTime, addUTCTime)
import System.Timeout (timeout)
import Test.QuickCheck hiding (replay, label)
import TestSupport
import TradingGame

-- No real sleeps: advancing one TVar changes both clock reads and all alarms.
manualClock :: IO (LiveClock, UTCTime -> IO ())
manualClock = do
  time <- newTVarIO simulationStart
  let clock = LiveClock
        (readTVarIO time)
        (\target -> pure (readTVar time >>= check . (>= target)))
      advance target = atomically $ do
        current <- readTVar time
        if target < current then error "test clock moved backwards"
          else writeTVar time target
  pure (clock, advance)

record :: TVar [LiveEvent] -> LiveEvent -> IO ()
record events event = atomically (modifyTVar' events (event :))

awaitTrace :: TVar [LiveEvent] -> ([LiveEvent] -> Bool) -> IO ()
awaitTrace events predicate = atomically (readTVar events >>= check . predicate)

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual = unless (expected == actual) $
  ioError (userError (label ++ ": expected " ++ show expected ++ ", got " ++ show actual))

-- Matching on the GADT recovers the Eq dictionary for each response type.
sameDecision :: TradingGame m a -> Decision a -> Decision a -> Bool
sameDecision request expected actual = case request of
  GetMyPrivateNumber -> expected == actual
  GetExchangeState -> expected == actual
  SubmitOrder _ -> expected == actual
  Wait _ -> expected == actual
  WaitUntil _ -> expected == actual
  AwaitSettlement -> expected == actual

replay :: Engine -> [LiveEvent] -> Either String Engine
replay = foldM step
  where
    step engine (ExchangeClosed now) = Right (advanceTo now engine)
    step engine (RequestHandled now pid request expected) =
      let (updated, actual) = handleRequest now pid request engine
      in if sameDecision request expected actual then Right updated
         else Left ("response mismatch at " ++ show now ++ " for " ++ show pid)

liveProperties :: [Property]
liveProperties = [prop_liveReplay]

-- Exercise real STM transport and compare its observed responses and final
-- state with pure replay. Ordering here is controlled; the player test below
-- also replays a trace produced by concurrent, time-dependent programs.
prop_liveReplay :: Property
prop_liveReplay = forAll (listOf ((,) <$> elements [PlayerId 1, PlayerId 2] <*> genOrder)) $ \orders ->
  ioProperty $ do
    result <- timeout 5000000 $ do
      (clock, advance) <- manualClock
      runtime <- newLiveRuntime 1
      events <- newTVarIO []
      Async.withAsync (runExchange clock (record events) runtime fixture) $ \server -> do
        private <- requestLive runtime (PlayerId 1) GetMyPrivateNumber
        assertEqual "private number" (Right (Reply 3)) private
        model <- foldM (\engine (index :: Integer, (pid, order)) -> do
          -- Several processing times, all strictly before the deadline.
          let now = addUTCTime (fromRational (toRational index / toRational (length orders + 1))) simulationStart
              (updated, expected) = handleRequest now pid (SubmitOrder order) engine
          advance now
          requestLive runtime pid (SubmitOrder order) >>= assertEqual "order reply" (Right expected)
          requestLive runtime pid GetExchangeState >>=
            assertEqual "snapshot reply" (Right (snd (handleRequest now pid GetExchangeState updated)))
          pure updated) fixture (zip [0..] orders)
        now <- clockNow clock
        requestLive runtime (PlayerId 1) (Wait (-1)) >>= assertEqual "negative wait" (Right (ResumeAt now))
        requestLive runtime (PlayerId 2) (WaitUntil simulationStart) >>= assertEqual "past wait" (Right (ResumeAt now))
        advance (closesAt (engineInfo fixture))
        final <- Async.wait server
        trace <- reverse <$> readTVarIO events
        assertEqual "model final state" (advanceTo (closesAt (engineInfo fixture)) model) final
        pure (replay fixture trace == Right final)
    pure (counterexample "live trace differed or runner timed out" (result == Just True))

hasWait :: PlayerId -> [LiveEvent] -> Bool
hasWait pid = any $ \event -> case event of
  RequestHandled _ who (Wait _) (ResumeAt _) -> who == pid
  _ -> False

hasSettlementWait :: PlayerId -> [LiveEvent] -> Bool
hasSettlementWait pid = any $ \event -> case event of
  RequestHandled _ who AwaitSettlement WhenResolved -> who == pid
  _ -> False

testConcurrentPlayers :: IO ()
testConcurrentPlayers = do
  (clock, advance) <- manualClock
  events <- newTVarIO []
  let buyer = do
        secret <- getMyPrivateNumber
        unless (secret == 3) (error "wrong private number")
        void (submitOrder (LimitOrder Buy (Price 5) 2))
        wait 10
        snapshot <- getExchangeState
        unless (tradeHistory snapshot == [Trade (Price 5) 2 (addUTCTime 5 simulationStart)]) $
          error "sleeping buyer did not observe seller's fill"
        void awaitSettlement
      seller = do
        wait 5
        void (submitOrder (LimitOrder Sell (Price 5) 2))
        void awaitSettlement
      -- Deliberately reverse ID order to check the runner's result ordering.
      players = [(PlayerId 2, seller, 7), (PlayerId 1, buyer, 3)]
      initial = newEngine simulationStart 60 [(PlayerId 2, 7), (PlayerId 1, 3)]
      config = defaultLiveConfig
        { liveDuration = 60, liveQueueCapacity = 1, onLiveEvent = record events }
  Async.withAsync (runLiveEngineWith clock config players) $ \game -> do
    awaitTrace events (\trace -> hasWait (PlayerId 1) trace && hasWait (PlayerId 2) trace)
    advance (addUTCTime 5 simulationStart)
    awaitTrace events (hasSettlementWait (PlayerId 2))
    advance (addUTCTime 10 simulationStart)
    awaitTrace events (hasSettlementWait (PlayerId 1))
    advance (addUTCTime 60 simulationStart)
    final <- Async.wait game
    assertEqual "settlements" [Settlement 10 (-10), Settlement 10 10] (engineSettlements final)
    trace <- reverse <$> readTVarIO events
    assertEqual "concurrent trace replay" (Right final) (replay initial trace)
    let settled = [pid | RequestHandled _ pid AwaitSettlement (Reply _) <- trace]
    assertEqual "all parked replies completed" [PlayerId 2, PlayerId 1] settled

testStopsAtClosure :: IO ()
testStopsAtClosure = do
  (clock, advance) <- manualClock
  events <- newTVarIO []
  let sleeper = wait 120 >> error "live runner resumed a wait beyond closure"
      config = defaultLiveConfig { liveDuration = 60, onLiveEvent = record events }
  Async.withAsync (runLiveWith clock config [(PlayerId 1, sleeper, 3), (PlayerId 2, pure (), 7)]) $ \game -> do
    awaitTrace events (hasWait (PlayerId 1))
    advance (addUTCTime 60 simulationStart)
    final <- Async.wait game
    assertEqual "idle closure" [Settlement 10 0, Settlement 10 0] final

-- Hold the first processed request in the host trace callback, then fill the
-- queue and start another writer. Shutdown must release both queued reply
-- waiters and writers; closure must not sit behind the saturated request queue.
testSaturatedShutdown :: Bool -> IO ()
testSaturatedShutdown failExchange = do
  (clock, advance) <- manualClock
  runtime <- newLiveRuntime 1
  entered <- newEmptyTMVarIO
  release <- newEmptyTMVarIO
  count <- newTVarIO (0 :: Int)
  let trace event = case event of
        RequestHandled _ _ _ _ -> do
          atomically (modifyTVar' count (+ 1) >> putTMVar entered ())
          atomically (readTMVar release)
          if failExchange then ioError (userError "exchange failed") else pure ()
        _ -> pure ()
      request = requestLive runtime (PlayerId 1) GetExchangeState
  Async.withAsync request $ \first ->
    Async.withAsync (runExchange clock trace runtime fixture) $ \server -> do
      atomically (readTMVar entered)
      Async.withAsync request $ \queued -> do
        atomically (isFullTBQueue (requestQueue runtime) >>= check)
        Async.withAsync request $ \writer -> do
          advance (addUTCTime 60 simulationStart)
          atomically (putTMVar release ())
          result <- Async.waitCatch server
          case (failExchange, result) of
            (True, Left _) -> pure ()
            (False, Right final) -> assertEqual "resolved" (Resolved 10) (enginePhase final)
            _ -> ioError (userError "unexpected exchange termination")
          firstResult <- Async.wait first
          if failExchange then assertEqual "in-flight failure" (Left LiveStopped) firstResult
            else case firstResult of
              Right (Reply snapshot) -> assertEqual "pre-closure snapshot" Trading (gamePhase snapshot)
              _ -> ioError (userError "first request did not receive its reply")
          Async.wait queued >>= assertEqual "queued reply released" (Left LiveStopped)
          Async.wait writer >>= assertEqual "blocked writer released" (Left LiveStopped)
          request >>= assertEqual "admission after stop" (Left LiveStopped)
          readTVarIO count >>= assertEqual "no processing after closure" 1

-- Advance time in the authoritative read after dequeueing. A request queued
-- before closure but processed at the deadline must go through GameClosed.
testDeadlineBetweenDequeueAndHandle :: IO ()
testDeadlineBetweenDequeueAndHandle = do
  (baseClock, advance) <- manualClock
  calls <- newTVarIO (0 :: Int)
  let clock = baseClock { clockNow = do
        call <- atomically $ do
          n <- readTVar calls
          writeTVar calls (n + 1)
          pure n
        unless (call == 0) (advance (addUTCTime 60 simulationStart))
        clockNow baseClock }
  runtime <- newLiveRuntime 1
  reply <- newEmptyTMVarIO
  atomically $ writeTBQueue (requestQueue runtime)
    (Request (PlayerId 1) (SubmitOrder (LimitOrder Buy (Price 5) 1)) reply)
  final <- runExchange clock (const (pure ())) runtime fixture
  atomically (readTMVar reply) >>= assertEqual "deadline rejection" (Reply (Left GameClosed))
  assertEqual "no late order ID allocated" 1 (nextOrderId (engineBook final))

testPlayerFailure :: Bool -> IO ()
testPlayerFailure inPayload = do
  (clock, advance) <- manualClock
  events <- newTVarIO []
  let failing = do
        wait 1
        if inPayload
          then void (submitOrder (LimitOrder Buy (Price (error "bad price")) 1))
          else error "player failed"
      config = defaultLiveConfig { liveDuration = 60, onLiveEvent = record events }
  Async.withAsync (runLiveWith clock config [(PlayerId 1, failing, 3), (PlayerId 2, wait 120, 7)]) $ \game -> do
    awaitTrace events (\trace -> hasWait (PlayerId 1) trace && hasWait (PlayerId 2) trace)
    advance (addUTCTime 1 simulationStart)
    result <- Async.waitCatch game
    case result of
      Left _ -> pure ()
      Right _ -> ioError (userError "worker exception was swallowed")
    trace <- readTVarIO events
    let orders :: [()]
        orders = [() | RequestHandled _ _ (SubmitOrder _) _ <- trace]
    assertEqual "failed payload stays in worker" [] orders

testExchangeFailureSupervision :: IO ()
testExchangeFailureSupervision = do
  (clock, _) <- manualClock
  let config = defaultLiveConfig { onLiveEvent = \_ -> throwIO (userError "trace failed") }
  result <- try (runLiveWith clock config [(PlayerId 1, void getExchangeState, 3)])
  case result of
    Left (_ :: SomeException) -> pure ()
    Right _ -> ioError (userError "exchange exception was swallowed")

testRealClock :: IO ()
testRealClock = do
  result <- runLiveFor 0.01 [(PlayerId 1, void awaitSettlement, 7)]
  assertEqual "real timer settlement" [Settlement 7 0] result

testInvalidCapacity :: IO ()
testInvalidCapacity = do
  result <- try (newLiveRuntime 0)
  case result of
    Left (_ :: SomeException) -> pure ()
    Right _ -> ioError (userError "zero-capacity queue accepted")

runLiveTests :: IO Bool
runLiveTests = and <$> forM
  [ ("concurrent players, waits, settlement and replay", testConcurrentPlayers)
  , ("idle closure and cancellation of long waits", testStopsAtClosure)
  , ("closure with a saturated queue", testSaturatedShutdown False)
  , ("exchange failure with a saturated queue", testSaturatedShutdown True)
  , ("deadline between dequeue and handling", testDeadlineBetweenDequeueAndHandle)
  , ("player evaluation failure", testPlayerFailure False)
  , ("player request payload failure", testPlayerFailure True)
  , ("exchange failure supervision", testExchangeFailureSupervision)
  , ("real clock smoke test", testRealClock)
  , ("invalid queue capacity", testInvalidCapacity)
  ] (\(name, action) -> do
      -- Timeouts are deadlock guards, never a way to order concurrent actions.
      result <- timeout 5000000 (try action)
      case result of
        Just (Right ()) -> putStrLn ("PASS: " ++ name) >> pure True
        Just (Left (err :: SomeException)) ->
          putStrLn ("FAIL: " ++ name ++ ": " ++ displayException err) >> pure False
        Nothing -> putStrLn ("FAIL: " ++ name ++ ": timed out") >> pure False)
