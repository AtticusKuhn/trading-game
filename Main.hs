{-# LANGUAGE DataKinds #-}

-- Run: nix run path:.
module Main where

import Control.Effect (run)
import Control.Monad (unless)
import qualified Data.Map.Strict as Map
import Data.List (foldl')
import System.Exit (exitFailure)
import Test.QuickCheck
import TradingGame
import TestSupport
import EngineTests (engineProperties, discoverLaws)
import LiveTests (liveProperties)
import InteractionTests (interactionProperties)
import SessionTests (sessionProperties)
import ConcurrentTests (concurrentProperties)
import SimulationTests (simulationProperties)
import WebTests (webProperties)
import ManageGamesTests (manageGamesProperties)
import RevealTests (revealProperties)

-- Eff programs have no Show instance. Print the secrets and resulting
-- settlements on failure; QuickCheck's replay seed reproduces the programs.
prop_settlementsSumToZero :: Property
prop_settlementsSumToZero = checkCoverage $
  forAllShow genPlayers (\programs -> "Private numbers: " ++ show (map (privateNumber . fst) programs)) $ \programs ->
    let settlements = run (runTradingGame programs)
        payoffs = map netPayoff settlements
    in cover 10 (any (/= 0) payoffs) "nonzero individual payoffs" $
       counterexample ("Settlements: " ++ show settlements) $
         conjoin $ (sum payoffs === 0) :
           [ conjoin
               [ playerResults result === zipWith PlayerResult (map fst programs) payoffs
               , resolutions result === Map.fromSet
                   (\asset -> resolve asset (map (privateNumber . fst) programs)) allInstruments
               ]
           | result <- settlements
           ]

-- A lone player's self-trades cancel out, so their payoff is always zero.
prop_singlePlayerSettlementIsZero :: Property
prop_singlePlayerSettlementIsZero =
  forAllShow genTradingGame (const "Generated player program (use replay seed to reproduce)") $ \player ->
    forAll (arbitrary :: Gen Int) $ \secret ->
      let settlements = run (runTradingGame [(testPlayer (PlayerId 42) secret, player)])
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
            initial = Exchange 1 (Map.fromSet (const []) allInstruments) [] (Map.singleton owner (Account 0 Map.empty Map.empty))
            submit state (oid, order) =
              matchOrder simulationStart owner oid order state
            final = foldl' submit initial (zip (map OrderId [1..]) orders)
            oneSide entries =
              let book = map snd entries
              in null (filter ((== Buy) . restingSide) book)
                 || null (filter ((== Sell) . restingSide) book)
        in counterexample ("Remaining books: " ++ show (books final)) $
             property (all oneSide (Map.elems (books final)))

main :: IO ()
main = do
  results <- mapM (quickCheckWithResult stdArgs { maxSuccess = 100 })
    ([ prop_settlementsSumToZero
    , prop_singlePlayerSettlementIsZero
    , prop_samePriceLeavesOnlyOneSide
    ] ++ engineProperties ++ liveProperties ++ interactionProperties ++ concurrentProperties ++ simulationProperties ++ sessionProperties ++ webProperties ++ manageGamesProperties ++ revealProperties)
  discoverLaws
  unless (all isSuccess results) exitFailure
