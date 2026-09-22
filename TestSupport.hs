{-# LANGUAGE DataKinds #-}

module TestSupport where

import Control.Effect (Eff)
import Control.Monad (void)
import Data.Ratio ((%))
import Test.QuickCheck
import TradingGame

-- Each player submits a variable-length list of orders, then waits for settlement.
genTradingGame :: Gen (Eff '[TradingGame, Concurrent] ())
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

genPlayers :: Gen [PlayerProgram '[]]
genPlayers = do
  count <- chooseInt (1, 8)
  ids <- take count <$> shuffle (map PlayerId [-20..20])
  mapM (\pid -> (,) <$> (testPlayer pid <$> chooseInt (-100, 100)) <*> genTradingGame) ids

-- A reachable pre-closure engine, built through the public request handler.
genEngine :: Gen Engine
genEngine = do
  roster <- genRoster
  orders <- listOf ((,) <$> elements (map playerID roster) <*> genOrder)
  let initial = newEngine simulationStart 60 roster
  pure (foldl (\engine (pid, order) ->
    fst (handleRequest simulationStart pid (SubmitOrder order) engine)) initial orders)

genRoster :: Gen [Player]
genRoster = do
  count <- chooseInt (1, 8)
  ids <- take count <$> shuffle (map PlayerId [-20..20])
  mapM (\pid -> Player pid <$> (((show pid ++ ":") ++) <$> arbitrary) <*> arbitrary) ids

-- Stable names for generated host identities.
testPlayer :: PlayerId -> Int -> Player
testPlayer pid secret = Player pid (show pid) (toInteger secret)
