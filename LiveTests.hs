{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LiveTests (liveProperties, manualClock, liveProperty) where

import qualified Control.Concurrent.Async as Async
import Control.Concurrent (yield)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (SomeException, fromException, throwIO, try)
import Control.Effect (runIO)
import Control.Monad (foldM, unless, void)
import Data.Ratio ((%))
import Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime)
import GHC.Conc (ThreadStatus(..), BlockReason(..), threadStatus)
import Test.QuickCheck hiding (replay, total)
import TestSupport (testPlayer)
import TradingGame

-- Positive gaps preserve distinct IDs and ordered wakeups even while shrinking.
data Scenario = Scenario
  { identities :: (Integer, Positive Integer)
  , secrets :: (Int, Int)
  , startOffset :: Integer
  , timeGaps :: (Positive Integer, Positive Integer, Positive Integer)
  } deriving Show

instance Arbitrary Scenario where
  arbitrary = Scenario <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink (Scenario ids numbers offset gaps) =
    [Scenario ids' numbers' offset' gaps' |
      (ids', numbers', offset', gaps') <- shrink (ids, numbers, offset, gaps)]

scenarioPlayers :: Scenario -> ((PlayerId, Int), (PlayerId, Int))
scenarioPlayers scenario =
  let (identifier, Positive gap) = identities scenario
      (firstSecret, secondSecret) = secrets scenario
  in ((PlayerId identifier, firstSecret), (PlayerId (identifier + gap), secondSecret))

scenarioStart :: Scenario -> UTCTime
scenarioStart scenario = addUTCTime (fromInteger (startOffset scenario)) simulationStart

-- First wakeup, second wakeup, closure; all strictly separated.
scenarioTimes :: Scenario -> (NominalDiffTime, NominalDiffTime, NominalDiffTime)
scenarioTimes scenario =
  let (Positive first, Positive second, Positive third) = timeGaps scenario
  in (fromInteger first, fromInteger (first + second), fromInteger (first + second + third))

scenarioEngine :: Scenario -> Engine
scenarioEngine scenario =
  let ((first, firstSecret), (second, secondSecret)) = scenarioPlayers scenario
      (_, _, duration) = scenarioTimes scenario
  in newEngine (scenarioStart scenario) duration
       [testPlayer first firstSecret, testPlayer second secondSecret]

-- Generate valid orders, including negative/fractional prices, with shrinking.
newtype ValidOrder = ValidOrder (Bool, Integer, Positive Integer, Positive Integer)
  deriving Show

instance Arbitrary ValidOrder where
  arbitrary = ValidOrder <$> arbitrary
  shrink (ValidOrder values) = map ValidOrder (shrink values)

validOrder :: ValidOrder -> LimitOrder
validOrder (ValidOrder (buy, numerator, Positive denominator, Positive quantity)) =
  LimitOrder (if buy then Buy else Sell) (Price (numerator % denominator)) quantity

-- No real sleeps: advancing one TVar changes both clock reads and all alarms.
manualClock :: UTCTime -> IO (LiveClock, UTCTime -> IO ())
manualClock start = do
  time <- newTVarIO start
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

-- Inputs are generated outside IO, so shrinking recreates the whole runtime.
-- This fixed timeout is a deadlock guard, never a way to order concurrent actions.
liveProperty :: String -> IO Property -> Property
liveProperty name action = counterexample name $ within 5000000 $ ioProperty action

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
liveProperties =
  [ property prop_liveReplay
  , property prop_concurrentPlayers
  , property prop_stopsAtClosure
  , property prop_concurrentOrders
  , property prop_settlementBroadcast
  , property prop_deadlineUnderContention
  , property prop_lockRecovery
  , property prop_exchangeFailureSupervision
  , prop_realClock
  ]

-- Compare direct live responses and final state with the pure engine.
prop_liveReplay :: Scenario -> [(Bool, ValidOrder)] -> Positive Integer -> Property
prop_liveReplay scenario orders (Positive past) = liveProperty "live replay" $ do
  let initial = scenarioEngine scenario
      start = scenarioStart scenario
      ((first, firstSecret), (second, _)) = scenarioPlayers scenario
      (_, _, duration) = scenarioTimes scenario
  (clock, advance) <- manualClock start
  events <- newTVarIO []
  runtime <- newLiveRuntime clock (record events) initial
  Async.withAsync (runExchange runtime) $ \server -> do
    private <- requestLive runtime first GetMyPrivateNumber
    (model, checks) <- foldM (\(engine, checks) (index :: Integer, (isFirst, generated)) -> do
      -- Spread requests across the generated duration, strictly before closure.
      let now = addUTCTime (duration * fromRational (index % toInteger (length orders + 1))) start
          pid = if isFirst then first else second
          order = validOrder generated
          (updated, expected) = handleRequest now pid (SubmitOrder order) engine
      advance now
      reply <- requestLive runtime pid (SubmitOrder order)
      snapshot <- requestLive runtime pid GetExchangeState
      pure (updated, conjoin
        [ counterexample "order reply" (reply === expected)
        , counterexample "snapshot reply" (snapshot === snd (handleRequest now pid GetExchangeState updated))
        ] : checks)) (initial, []) (zip [0..] orders)
    now <- clockNow clock
    negativeWait <- requestLive runtime first (Wait (negate (fromInteger past)))
    pastWait <- requestLive runtime second (WaitUntil (addUTCTime (negate (fromInteger past)) start))
    advance (closesAt (engineInfo initial))
    final <- Async.wait server
    trace <- reverse <$> readTVarIO events
    pure $ conjoin (checks ++
      [ private === Reply (toInteger firstSecret)
      , negativeWait === ResumeAt now
      , pastWait === ResumeAt now
      , counterexample "model final state" (final === advanceTo (closesAt (engineInfo initial)) model)
      , counterexample "trace replay" (replay initial trace === Right final)
      ])

hasWait :: PlayerId -> [LiveEvent] -> Bool
hasWait pid = any $ \event -> case event of
  RequestHandled _ who (Wait _) (ResumeAt _) -> who == pid
  _ -> False

hasSettlementWait :: PlayerId -> [LiveEvent] -> Bool
hasSettlementWait pid = any $ \event -> case event of
  RequestHandled _ who AwaitSettlement WhenResolved -> who == pid
  _ -> False

prop_concurrentPlayers :: Scenario -> ValidOrder -> Property
prop_concurrentPlayers scenario generated = liveProperty "concurrent programs, waits, settlement and replay" $ do
  let start = scenarioStart scenario
      ((buyerId, buyerSecret), (sellerId, sellerSecret)) = scenarioPlayers scenario
      (sellerWake, buyerWake, duration) = scenarioTimes scenario
      order = validOrder generated
      Price price = limitPrice order
      quantity = orderQuantity order
      total = toInteger buyerSecret + toInteger sellerSecret
      payoff = fromInteger quantity * (fromInteger total - price)
  (clock, advance) <- manualClock start
  events <- newTVarIO []
  let buyer = do
        secret <- getMyPrivateNumber
        unless (secret == toInteger buyerSecret) (error "wrong private number")
        void (submitOrder order { orderSide = Buy })
        wait buyerWake
        snapshot <- getExchangeState
        unless (tradeHistory snapshot == [Trade (limitPrice order) quantity (addUTCTime sellerWake start)]) $
          error "sleeping buyer did not observe seller's fill"
        void awaitSettlement
      seller = do
        wait sellerWake
        void (submitOrder order { orderSide = Sell })
        void awaitSettlement
      -- Reverse ID order to check that settlements follow the input order.
      results = [PlayerResult (testPlayer sellerId sellerSecret) (-payoff),
                 PlayerResult (testPlayer buyerId buyerSecret) payoff]
      programs = [(testPlayer sellerId sellerSecret, seller), (testPlayer buyerId buyerSecret, buyer)]
      initial = newEngine start duration [testPlayer sellerId sellerSecret, testPlayer buyerId buyerSecret]
      config = defaultLiveConfig { liveDuration = duration, onLiveEvent = record events }
  Async.withAsync (runIO (runConcurrent (runLiveEngineWith clock config programs))) $ \game -> do
    awaitTrace events (\trace -> hasWait buyerId trace && hasWait sellerId trace)
    advance (addUTCTime sellerWake start)
    awaitTrace events (hasSettlementWait sellerId)
    advance (addUTCTime buyerWake start)
    awaitTrace events (hasSettlementWait buyerId)
    advance (addUTCTime duration start)
    final <- Async.wait game
    trace <- reverse <$> readTVarIO events
    pure $ conjoin
      [ engineSettlements final ===
          [Settlement total (-payoff) results, Settlement total payoff results]
      , counterexample "concurrent trace replay" (replay initial trace === Right final)
      , counterexample "one closure event" (length [() | ExchangeClosed _ <- trace] === 1)
      ]

prop_stopsAtClosure :: Scenario -> Positive Integer -> Property
prop_stopsAtClosure scenario (Positive overrun) = liveProperty "idle closure and cancellation of long waits" $ do
  let ((first, firstSecret), (second, secondSecret)) = scenarioPlayers scenario
      start = scenarioStart scenario
      (_, _, duration) = scenarioTimes scenario
      total = toInteger firstSecret + toInteger secondSecret
      results = [PlayerResult (testPlayer first firstSecret) 0,
                 PlayerResult (testPlayer second secondSecret) 0]
  (clock, advance) <- manualClock start
  events <- newTVarIO []
  let sleeper = wait (duration + fromInteger overrun) >> error "live runner resumed a wait beyond closure"
      config = defaultLiveConfig { liveDuration = duration, onLiveEvent = record events }
  Async.withAsync (runIO (runConcurrent (runLiveWith clock config [(testPlayer first firstSecret, sleeper), (testPlayer second secondSecret, pure ())]))) $ \game -> do
    awaitTrace events (hasWait first)
    advance (addUTCTime duration start)
    final <- Async.wait game
    pure (final === [Settlement total 0 results, Settlement total 0 results])

-- All submissions compete for the same engine; no fills or updates may be lost.
prop_concurrentOrders :: Scenario -> ValidOrder -> Positive Int -> Property
prop_concurrentOrders scenario generated (Positive count) = liveProperty "concurrent order updates" $ do
  let initial = scenarioEngine scenario
      ((buyer, _), (seller, _)) = scenarioPlayers scenario
      order = validOrder generated
      Price price = limitPrice order
      volume = toInteger count * orderQuantity order
  (clock, advance) <- manualClock (scenarioStart scenario)
  events <- newTVarIO []
  runtime <- newLiveRuntime clock (record events) initial
  let submit (pid, side) = requestLive runtime pid (SubmitOrder order { orderSide = side })
  replies <- Async.mapConcurrently submit (concat (replicate count [(buyer, Buy), (seller, Sell)]))
  advance (closesAt (engineInfo initial))
  final <- runExchange runtime
  trace <- reverse <$> readTVarIO events
  pure $ conjoin
    [ counterexample "all orders accepted" (property (all accepted replies))
    , length (executedTrades (engineBook final)) === count
    , accounts (engineBook final) === [(buyer, (volume, -fromInteger volume * price)), (seller, (-volume, fromInteger volume * price))]
    , nextOrderId (engineBook final) === nextOrderId (engineBook initial) + 2 * toInteger count
    , counterexample "serialized trace" (replay initial trace === Right final)
    ]
  where
    accepted (Reply (Right _)) = True
    accepted _ = False

-- Let both settlement continuations finish without high-level cancellation.
prop_settlementBroadcast :: Scenario -> ValidOrder -> Property
prop_settlementBroadcast scenario generated = liveProperty "shared settlement notification" $ do
  let initial = scenarioEngine scenario
      ((buyer, buyerSecret), (seller, sellerSecret)) = scenarioPlayers scenario
      order = validOrder generated
      Price price = limitPrice order
      total = engineTotal initial
      payoff = fromInteger (orderQuantity order) * (fromInteger total - price)
  (clock, advance) <- manualClock (scenarioStart scenario)
  events <- newTVarIO []
  runtime <- newLiveRuntime clock (record events) initial
  let close = closesAt (engineInfo initial)
      results = [PlayerResult (testPlayer buyer buyerSecret) payoff,
                 PlayerResult (testPlayer seller sellerSecret) (-payoff)]
      player pid side secret expected = (testPlayer pid secret, do
        void (submitOrder order { orderSide = side })
        result <- awaitSettlement
        unless (result == Settlement total expected results) (error "wrong shared settlement"))
      programs = [player buyer Buy buyerSecret payoff, player seller Sell sellerSecret (-payoff)]
  Async.withAsync (Async.mapConcurrently_ (runIO . runLivePlayer close runtime) programs) $ \workers -> do
    awaitTrace events (\trace -> all (`hasSettlementWait` trace) [buyer, seller])
    advance close
    final <- runExchange runtime
    Async.wait workers
    published <- atomically (readTMVar (runtimeFinal runtime))
    pure (published === final)

-- A caller blocked on the engine must sample time after acquiring it.
-- No deadline worker is running: the request itself must resolve the game.
prop_deadlineUnderContention :: Scenario -> ValidOrder -> NonNegative Integer -> Property
prop_deadlineUnderContention scenario generated (NonNegative lateness) = liveProperty "deadline under lock contention" $ do
  let initial = scenarioEngine scenario
      ((pid, _), _) = scenarioPlayers scenario
      order = validOrder generated
  (clock, advance) <- manualClock (scenarioStart scenario)
  runtime <- newLiveRuntime clock (const (pure ())) initial
  locked <- takeMVar (runtimeEngine runtime)
  let request = requestLive runtime pid (SubmitOrder order)
      awaitBlocked caller = do
        status <- threadStatus (Async.asyncThreadId caller)
        unless (status == ThreadBlocked BlockedOnMVar) (yield >> awaitBlocked caller)
  rejected <- Async.withAsync request $ \caller -> do
    awaitBlocked caller
    advance (addUTCTime (fromInteger lateness) (closesAt (engineInfo initial)))
    putMVar (runtimeEngine runtime) locked
    Async.wait caller
  final <- atomically (readTMVar (runtimeFinal runtime))
  afterClosure <- request
  pure $ conjoin
    [ rejected === Reply (Left GameClosed)
    , nextOrderId (engineBook final) === nextOrderId (engineBook initial)
    , afterClosure === Reply (Left GameClosed)
    ]

-- Match the injected exception, including linked-worker wrappers. A deadlock
-- timeout or unrelated exception must never count as successful supervision.
isTestFailure :: String -> SomeException -> Bool
isTestFailure message err = case fromException err of
  Just (Async.ExceptionInLinkedThread _ cause) -> isTestFailure message cause
  Nothing -> fromException err == Just (userError message)

prop_lockRecovery :: Scenario -> ValidOrder -> Property
prop_lockRecovery scenario generated = liveProperty "lock recovery after a callback exception" $ do
  let initial = scenarioEngine scenario
      ((first, _), (second, _)) = scenarioPlayers scenario
  (clock, _) <- manualClock (scenarioStart scenario)
  let trace event = case event of
        RequestHandled _ pid _ _ | pid == first -> throwIO (userError "trace failed")
        _ -> pure ()
  runtime <- newLiveRuntime clock trace initial
  let submit pid = requestLive runtime pid (SubmitOrder (validOrder generated))
  result <- try (submit first)
  restored <- readMVar (runtimeEngine runtime)
  next <- submit second
  pure $ conjoin
    [ counterexample "trace exception propagated" (property (either (isTestFailure "trace failed") (const False) result))
    , counterexample "rollback" (restored === initial)
    , counterexample "lock released" (next === Reply (Right (OrderId (nextOrderId (engineBook initial)))))
    ]
  
prop_exchangeFailureSupervision :: Scenario -> Property
prop_exchangeFailureSupervision scenario = liveProperty "exchange failure supervision" $ do
  let ((pid, secret), _) = scenarioPlayers scenario
      (_, _, duration) = scenarioTimes scenario
  (clock, _) <- manualClock (scenarioStart scenario)
  let config = defaultLiveConfig
        { liveDuration = duration, onLiveEvent = \_ -> throwIO (userError "trace failed") }
  result <- try (runIO (runConcurrent (runLiveWith clock config [(testPlayer pid secret, void getExchangeState)])))
  pure $ counterexample "exchange exception propagated" $
    property (either (isTestFailure "trace failed") (const False) result)

prop_realClock :: Property
prop_realClock = forAllShrink arbitrary shrink $ \(identifier, secret) ->
  -- Bound wall-clock cost to 10 ms per case; still exercise a positive real timer.
  forAllShrink (chooseInteger (1, 10000)) (filter (> 0) . shrink) $ \micros ->
    liveProperty "real clock settlement" $ do
      result <- runIO $ runConcurrent $ runLiveFor (fromRational (micros % 1000000))
        [(testPlayer (PlayerId identifier) secret, void awaitSettlement)]
      pure (result === [Settlement (toInteger secret) 0 [PlayerResult (testPlayer (PlayerId identifier) secret) 0]])
