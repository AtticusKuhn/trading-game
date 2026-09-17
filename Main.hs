{-# LANGUAGE DataKinds #-}

-- Run: nix run path:.
module Main where

import Control.Monad (unless, void)
import Data.List (foldl')
import System.Exit (exitFailure)
import Test.QuickCheck
import TradingGame
import TestSupport
import EngineTests (engineProperties, discoverLaws)
import LiveTests (liveProperties)

-- Eff programs have no Show instance. Print the secrets and resulting
-- settlements on failure; QuickCheck's replay seed reproduces the programs.
prop_settlementsSumToZero :: Property
prop_settlementsSumToZero = checkCoverage $
  forAllShow genPlayers (\players -> "Private numbers: " ++ show [(pid, secret) | (pid, _, secret) <- players]) $ \players ->
    let settlements = runTradingGame players
        payoffs = map netPayoff settlements
    in cover 10 (any (/= 0) payoffs) "nonzero individual payoffs" $
       counterexample ("Settlements: " ++ show settlements) $
         sum payoffs === 0

-- A lone player's self-trades cancel out, so their payoff is always zero.
prop_singlePlayerSettlementIsZero :: Property
prop_singlePlayerSettlementIsZero =
  forAllShow genTradingGame (const "Generated player program (use replay seed to reproduce)") $ \player ->
    forAll (arbitrary :: Gen Int) $ \secret ->
      let settlements = runTradingGame [(PlayerId 42, player, secret)]
      in counterexample ("Settlements: " ++ show settlements) $
           map netPayoff settlements === [0]

-- Check matching before expiry: settlement itself unconditionally clears the
-- book. Self-trades are allowed, so all orders can belong to one player.
prop_samePriceLeavesOnlyOneSide :: Property
prop_samePriceLeavesOnlyOneSide =
  forAll (limitPrice <$> genOrder) $ \price ->
    forAllShrink (listOf ((\order -> order { limitPrice = price }) <$> genOrder))
      (shrinkList (const [])) $ \orders ->
        let owner = PlayerId 1
            initial = Exchange 1 [] [] [(owner, (0, 0))]
            submit state (oid, order) =
              matchOrder simulationStart owner oid order state
            final = foldl' submit initial (zip (map OrderId [1..]) orders)
            book = map snd (ownedOrders final)
            buys = filter ((== Buy) . restingSide) book
            sells = filter ((== Sell) . restingSide) book
        in counterexample ("Remaining book: " ++ show book) $
             property (null buys || null sells)

-- Explicit IDs survive the auxiliary runner, and settlements follow input order.
prop_explicitPlayerIds :: Property
prop_explicitPlayerIds =
  let buyer = PlayerId 42
      seller = PlayerId (-7)
      buy = void (submitOrder (LimitOrder Buy (Price 5) 2))
      sell = void (submitOrder (LimitOrder Sell (Price 5) 2))
      players = [(buyer, buy, 3), (seller, sell, 7)]
      final = runTradingGameAt' simulationStart 3600 players
      expected = [Settlement 10 10, Settlement 10 (-10)]
  in conjoin
       [ accounts final === [(buyer, (2, -10)), (seller, (-2, 10))]
       , runTradingGame players === expected
       , runTradingGameFor 60 players === expected
       , runTradingGameAt simulationStart 60 (reverse players) === reverse expected
       ]

main :: IO ()
main = do
  results <- mapM (quickCheckWithResult stdArgs { maxSuccess = 100 })
    ([ prop_settlementsSumToZero
    , prop_singlePlayerSettlementIsZero
    , prop_samePriceLeavesOnlyOneSide
    , prop_explicitPlayerIds
    ] ++ engineProperties ++ liveProperties)
  discoverLaws
  unless (all isSuccess results) exitFailure
