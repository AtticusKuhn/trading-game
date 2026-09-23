{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module RevealTests (revealProperties) where

import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM
import Control.Effect (run)
import qualified Control.Effect.State.Strict as State
import Control.Monad (void)
import Data.ByteString.Builder (toLazyByteString)
import Data.List (find, nub, sort)
import Data.Ratio ((%))
import Data.Time.Clock (addUTCTime, diffUTCTime)
import LiveTests (liveProperty, manualClock)
import Network.Wai (responseToStream)
import System.Random (mkStdGen)
import Test.QuickCheck
import TestSupport (genRevealEngine, genRoster)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import TradingGame
import TradingGame.Terminal (renderInfo)
import TradingGame.Web

revealProperties :: [Property]
revealProperties =
  [ property prop_defaultSchedule, prop_publicPrefix, prop_futurePrivacy
  , prop_simulatedWait, prop_simulatedUpdates, prop_liveWait, prop_idleStream
  , property prop_sampling, prop_simulationDefaults
  ]

prop_defaultSchedule :: Integer -> Positive Integer -> Positive Int -> Property
prop_defaultSchedule offset (Positive duration) (Positive count) =
  let start = addUTCTime (fromInteger offset) simulationStart
      times = defaultRevealTimes start (addUTCTime (fromInteger duration) start) count
  in conjoin
    [ map (`diffUTCTime` start) times ===
        [fromRational (duration * toInteger i % toInteger (count + 1)) | i <- [1..count]]
    , times === sort times
    , length times === count
    ]

-- Advancing time publishes exactly the due prefix, including duplicates, and
-- splitting an advance cannot change the result or publish any event twice.
prop_publicPrefix :: Property
prop_publicPrefix = forAll genRevealEngine $ \engine ->
  forAll (arbitrary :: Gen (NonNegative Integer, NonNegative Integer)) $ \(NonNegative a, NonNegative b) ->
    let start = opensAt (engineInfo engine)
        early = addUTCTime (fromInteger a) start
        later = addUTCTime (fromInteger (a + b)) start
        final = advanceTo later engine
        snapshot = exchangeSnapshot later final
    in conjoin
      [ revealedNumbers snapshot === [value | (time, value) <- pendingReveals engine, time <= later]
      , revealTimes (gameInfo snapshot) === map fst (pendingReveals engine)
      , advanceTo later (advanceTo early engine) === final
      , advanceTo later final === final
      , players final === players engine
      ]

-- Changing only still-hidden values cannot change anything a player sees,
-- including rendered HTML and terminal output.
prop_futurePrivacy :: Property
prop_futurePrivacy = forAll genRevealEngine $ \engine -> forAll arbitrary $ \secret ->
  forAll (arbitrary :: Gen (NonNegative Integer)) $ \(NonNegative offset) ->
    let now = addUTCTime (fromInteger offset) (opensAt (engineInfo engine))
        current = advanceTo now engine
        changed = current { pendingReveals = [(time, secret) | (time, _) <- pendingReveals current] }
        before = exchangeSnapshot now current
        after = exchangeSnapshot now changed
    in conjoin
      [ before === after
      , renderHtml (exchangeView before Nothing) === renderHtml (exchangeView after Nothing)
      , renderInfo (ExchangeSnapshot before) === renderInfo (ExchangeSnapshot after)
      ]

-- Await skips past and simultaneous-at-call events, advances directly to the
-- next time, and returns immediately without advancing when exhausted.
prop_simulatedWait :: Property
prop_simulatedWait = forAll genRevealEngine $ \engine ->
  forAll (arbitrary :: Gen (NonNegative Integer)) $ \(NonNegative offset) ->
    let now = addUTCTime (fromInteger offset) (opensAt (engineInfo engine))
        next = find ((> now) . fst) (pendingReveals engine)
        target = maybe now fst next
        pid = playerID (head (players engine))
        (stopped, final, result) = run (runSimulatedPlayer now engine pid awaitUntilNextReveal)
    in conjoin
      [ stopped === target, result === fmap snd next
      , final === advanceTo target engine
      , snd (handleRequest now pid AwaitUntilNextReveal engine) ===
          maybe (Reply Nothing) (uncurry WhenRevealed) next
      ]

-- Virtual time drives idle subscribers too; coincident reveals produce one
-- snapshot containing the entire group, and repeated values remain events.
prop_simulatedUpdates :: Property
prop_simulatedUpdates = forAll genRevealEngine $ \engine ->
  let start = opensAt (engineInfo engine)
      watch snapshot = do
        State.modify (++ [snapshot])
        case gamePhase snapshot of
          Trading -> awaitExchangeChange snapshot >>= watch
          Resolved _ -> pure ()
      (snapshots, _) = run $ State.runState [] $ runSimulatedPlayer start engine
        (playerID (head (players engine))) (getExchangeState >>= watch)
      times = nub (start : revealTimes (engineInfo engine) ++ [closesAt (engineInfo engine)])
  in snapshots === [exchangeSnapshot time (advanceTo time engine) | time <- times]

-- All waiting players receive the same event even when the clock jumps past
-- additional events (or settlement) before their continuations can run.
prop_liveWait :: Property
prop_liveWait = forAll genRevealEngine $ \engine ->
  forAll (arbitrary :: Gen (NonNegative Integer)) $ \(NonNegative delay) ->
    liveProperty "reveal wait broadcast" $ do
      let start = opensAt (engineInfo engine)
          next = find ((> start) . fst) (pendingReveals engine)
          roster = players engine
      (clock, advance) <- manualClock start
      subscribed <- newTVarIO (0 :: Int)
      let trace (RequestHandled _ _ AwaitUntilNextReveal (WhenRevealed _ _)) =
            atomically (modifyTVar' subscribed (+1))
          trace _ = pure ()
      runtime <- newLiveRuntime clock trace engine
      results <- Async.withAsync (Async.mapConcurrently
        (\player -> handleLiveRequest runtime (playerID player) AwaitUntilNextReveal) roster) $ \listeners -> do
          case next of
            Nothing -> pure ()
            Just (time, _) -> do
              atomically (readTVar subscribed >>= check . (== length roster))
              advance (addUTCTime (fromInteger delay) time)
          Async.wait listeners
      pure (results === replicate (length roster) (fmap snd next))

-- No orders or polling: the exchange clock alone wakes an idle SSE connection.
-- A reconnect at that same instant also sees the complete reveal history.
prop_idleStream :: Property
prop_idleStream = forAll (genRevealEngine `suchThat` (not . null . future)) $ \engine ->
  forAll (elements (future engine)) $ \(time, _) -> liveProperty "idle reveal SSE" $ do
    let start = opensAt (engineInfo engine)
        player = head (players engine)
        expected = exchangeSnapshot time (advanceTo time engine)
        chunk = toLazyByteString (sseHtml "exchange" (exchangeView expected Nothing))
    (clock, advance) <- manualClock start
    runtime <- newLiveRuntime clock (const (pure ())) engine
    withWebGame runtime (players engine) [] $ \game -> do
      chunks <- newTVarIO []
      let (_, _, stream) = responseToStream (eventStream game (BrowserSession player))
          write value = atomically (modifyTVar' chunks (++ [toLazyByteString value]))
      Async.withAsync (runExchange runtime) $ \_ ->
        Async.withAsync (stream (\body -> body write (pure ()))) $ \_ -> do
          atomically (readTVar chunks >>= check . not . null)
          advance time
          atomically (readTVar chunks >>= check . elem chunk)
          snapshot <- handleLiveRequest runtime (playerID player) GetExchangeState
          pure (snapshot === expected)
  where future engine = filter ((> opensAt (engineInfo engine)) . fst) (pendingReveals engine)

-- Statistical distribution property: every roster slot has equal weight;
-- equal-valued slots contribute their combined weight. Eight standard
-- deviations gives a conservative guard against random false positives.
prop_sampling :: Int -> Property
prop_sampling seed = forAll genRoster $ \roster ->
  let draws = 8192
      plan = sampleRevealTargets (mkStdGen seed) roster (replicate draws simulationStart)
      ids = map snd plan
      count predicate = fromIntegral (length (filter predicate ids)) :: Double
      checkFrequency slots predicate =
        let expected = fromIntegral draws * fromIntegral slots / fromIntegral (length roster)
        in counterexample (show (expected, count predicate)) $
             property (abs (count predicate - expected) <= 8 * sqrt expected)
      valueIDs value = [playerID player | player <- roster, privateNumber player == value]
  in conjoin
    [ length plan === draws
    , conjoin [checkFrequency 1 (== playerID player) | player <- roster]
    , conjoin [checkFrequency (length (valueIDs value)) (`elem` valueIDs value)
              | value <- nub (map privateNumber roster)]
    ]

prop_simulationDefaults :: Property
prop_simulationDefaults = forAll genRoster $ \roster ->
  forAll (arbitrary :: Gen (Positive Integer)) $ \(Positive duration) ->
    let program = void awaitSettlement
        final = run (simulatePlayers simulationStart (fromInteger duration) [(player, program) | player <- roster])
    in conjoin
      [ length (engineRevealedNumbers final) === length roster
      , revealTimes (engineInfo final) === defaultRevealTimes simulationStart
          (addUTCTime (fromInteger duration) simulationStart) (length roster)
      , property (all (`elem` map privateNumber roster) (engineRevealedNumbers final))
      ]
