{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module SimulationTests (simulationProperties) where

import Control.Effect (run)
import qualified Control.Effect.State.Strict as State
import Control.Monad (void)
import Data.List (transpose)
import qualified Data.Time.Clock
import Test.QuickCheck
import TestSupport (genRoster, genOrder, genEngine)
import TradingGame

simulationProperties :: [Property]
simulationProperties =
  [ property prop_roundRobin
  , property prop_cancelDescendants
  , property prop_localHandlers
  , property prop_updates
  , property prop_resume
  , property prop_staleSnapshot
  ]

-- Workers take turns at each trading request and all keep their owner's secret.
-- Their number never affects the fixed roster or its settlements.
prop_roundRobin :: [Int] -> [[Int]] -> Positive Integer -> Property
prop_roundRobin bodyValues streams (Positive duration) = forAll genRoster $ \roster ->
  let owner = head roster
      worker values = mapM_ (\value -> getMyPrivateNumber >>= \secret ->
        State.modify ( ++ [(value, secret)])) values
      program = withWorkers (map worker streams) (worker bodyValues >> wait (fromInteger duration))
      (trace, results) = run $ State.runState [] $ runTradingGameFor (fromInteger duration)
        ((owner, program) : [(player, pure ()) | player <- tail roster])
  in conjoin
    [ trace === [(value, privateNumber owner) | value <- concat (transpose (bodyValues:streams))]
    , map (map settledPlayer . playerResults) results === replicate (length roster) roster
    ]

-- Returning cancels both sleeping and subscribed descendants, including nested
-- scopes. Later time advancement must not resurrect a cancelled continuation.
prop_cancelDescendants :: Positive Integer -> Positive Integer -> Integer -> Property
prop_cancelDescendants (Positive early) (Positive gap) value = forAll genRoster $ \roster ->
  let later = fromInteger (early + gap)
      mark = State.modify @[Integer] (value:)
      nested = withWorkers [wait later >> mark] (void awaitSettlement >> mark)
      subscribed = getExchangeState >>= awaitExchangeChange >> mark
      program = do
        result <- withWorkers [nested, subscribed] (wait (fromInteger early) >> pure value)
        wait later
        State.modify (result:)
      (trace, _) = run $ State.runState [] $
        runTradingGameFor later [(head roster, program)]
  in trace === [value]

-- Captured worker continuations retain handlers installed at the spawn site.
prop_localHandlers :: Integer -> [Integer] -> Positive Integer -> Property
prop_localHandlers initial values (Positive duration) = forAll genRoster $ \roster ->
  let worker = mapM_ (\value -> do
        void getMyPrivateNumber
        State.modify @Integer (+ value)
        local <- State.get @Integer
        State.modify @[Integer] (local:)) values
      program = void $ State.runState initial $ withWorkers [worker] (wait (fromInteger duration))
      (trace, _) = run $ State.runState [] $
        runTradingGameFor (fromInteger duration) [(head roster, program)]
  in trace === reverse (tail (scanl (+) initial values))

-- A sleeping command loop still receives both an order update and closure.
prop_updates :: Positive Integer -> Property
prop_updates (Positive gap) = forAll genRoster $ \roster -> forAll genOrder $ \order ->
  let spacing = fromInteger gap
      duration = spacing + spacing
      player = head roster
      watch snapshot = do
        next <- awaitExchangeChange snapshot
        State.modify @[ExchangeState] (++ [next])
        case gamePhase next of
          Trading -> watch next
          Resolved _ -> pure ()
      producer = wait spacing >> submitOrder order >> wait duration
      program = withWorkers [getExchangeState >>= watch] producer
      (trace, _) = run $ State.runState [] $ runTradingGameFor duration [(player, program)]
      placedAt = Data.Time.Clock.addUTCTime spacing simulationStart
      closedAt = Data.Time.Clock.addUTCTime duration simulationStart
      initial = newEngine simulationStart duration [player]
      updated = fst (handleRequest placedAt (playerID player) (SubmitOrder order) initial)
  in trace === [exchangeSnapshot placedAt updated, exchangeSnapshot closedAt (advanceTo closedAt updated)]

-- Changes between taking a snapshot and subscribing are returned immediately.
prop_staleSnapshot :: Positive Integer -> Property
prop_staleSnapshot (Positive duration) = forAll genRoster $ \roster -> forAll genOrder $ \order ->
  let program = do
        before <- getExchangeState
        void (submitOrder order)
        after <- awaitExchangeChange before
        State.put (Just after)
      (observed, _) = run $ State.runState Nothing $
        runTradingGameFor (fromInteger duration) [(head roster, program)]
      initial = newEngine simulationStart (fromInteger duration) [head roster]
      updated = fst (handleRequest simulationStart (playerID (head roster)) (SubmitOrder order) initial)
  in observed === Just (exchangeSnapshot simulationStart updated)

-- Pausing for logout and resuming the same account agrees with uninterrupted
-- execution, including waits/orders that cross the deadline.
prop_resume :: NonNegative Integer -> NonNegative Integer -> Property
prop_resume (NonNegative before) (NonNegative after) = forAll genEngine $ \engine ->
  forAll genOrder $ \order ->
    let pid = playerID (head (players engine))
        start = opensAt (engineInfo engine)
        first = wait (fromInteger before) >> submitOrder order
        second = wait (fromInteger after) >> getExchangeState
        whole = run (runSimulatedPlayer start engine pid (first >> second))
        split = run $ do
          (now, updated, _) <- runSimulatedPlayer start engine pid first
          runSimulatedPlayer now updated pid second
    in split === whole
