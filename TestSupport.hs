{-# LANGUAGE DataKinds #-}

module TestSupport where

import Control.Effect (Eff)
import Control.Monad (void)
import Data.Ratio ((%))
import Test.QuickCheck
import TradingGame

-- Each player submits a variable-length list of orders, then waits for settlement.
genTradingGame :: Gen (Eff '[TradingGame] ())
genTradingGame = do
  orders <- listOf genOrder
  pure $ mapM_ submitOrder orders >> void awaitSettlement

genOrder :: Gen LimitOrder
genOrder = do
  side <- elements [Buy, Sell]
  numerator <- chooseInteger (-20, 20)
  denominator <- chooseInteger (1, 4)
  quantity <- chooseInteger (1, 10)
  pure (LimitOrder side (Price (numerator % denominator)) quantity)

genPlayers :: Gen [(PlayerId, Eff '[TradingGame] (), Int)]
genPlayers = do
  count <- chooseInt (1, 8)
  ids <- take count <$> shuffle (map PlayerId [-20..20])
  mapM (\pid -> (,,) pid <$> genTradingGame <*> chooseInt (-100, 100)) ids

-- A reachable pre-closure engine, built through the public request handler.
genEngine :: Gen Engine
genEngine = do
  secrets <- vectorOf 3 (chooseInteger (-100, 100))
  orders <- listOf ((,) <$> elements [PlayerId 1, PlayerId 2, PlayerId 3] <*> genOrder)
  let initial = newEngine simulationStart 60 (zip (map PlayerId [1..3]) secrets)
  pure (foldl (\engine (pid, order) ->
    fst (handleRequest simulationStart pid (SubmitOrder order) engine)) initial orders)

fixture :: Engine
fixture = newEngine simulationStart 60 [(PlayerId 1, 3), (PlayerId 2, 7)]
