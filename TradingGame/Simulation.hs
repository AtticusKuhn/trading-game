{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module TradingGame.Simulation where

import Control.Effect (Eff)
import Data.List (sortOn)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), NominalDiffTime)
import TradingGame.Core
import TradingGame.Player

data Event effs
  = CloseExchange
  | ResumePlayer PlayerId (Eff (TradingGame ': effs) ())

simulationStart :: UTCTime
simulationStart = UTCTime (fromGregorian 2000 1 1) 0

-- Supply unique player IDs. Settlements follow the input player order.
runTradingGame :: [PlayerProgram effs] -> Eff effs [Settlement]
runTradingGame = runTradingGameFor 3600

runTradingGameFor :: NominalDiffTime -> [PlayerProgram effs] -> Eff effs [Settlement]
runTradingGameFor = runTradingGameAt simulationStart

runTradingGameAt :: UTCTime -> NominalDiffTime -> [PlayerProgram effs] -> Eff effs [Settlement]
runTradingGameAt start duration = fmap engineSettlements . simulatePlayers start duration

runTradingGameAt' :: UTCTime -> NominalDiffTime -> [PlayerProgram effs] -> Eff effs Exchange
runTradingGameAt' start duration = fmap engineBook . simulatePlayers start duration

simulatePlayers :: UTCTime -> NominalDiffTime -> [PlayerProgram effs] -> Eff effs Engine
simulatePlayers start duration programs =
  let initial = newEngine start duration
        (map fst programs)
      -- Stable sorting keeps closure ahead of wake-ups at the deadline.
      events = (closesAt (engineInfo initial), CloseExchange) :
        [(start, ResumePlayer (playerID player) program) | (player, program) <- programs]
  in simulate initial (sortOn fst events)

-- Each request yields to players already runnable at the same virtual time.
-- Infinite immediate requests prevent virtual time from advancing. Waits beyond
-- closure are honored; this runner finishes only when player activity finishes.
simulate :: Engine -> [(UTCTime, Event effs)] -> Eff effs Engine
simulate engine [] = pure engine
simulate engine ((now, event) : pending) =
  let current = advanceTo now engine
  in case event of
    CloseExchange -> simulate current pending
    ResumePlayer pid program -> do
      step <- stepPlayer program
      case step of
        Finished -> simulate current pending
        Requested request resume ->
          let (updated, decision) = handleRequest now pid request current
              schedule wake next = simulate updated $ sortOn fst
                (pending ++ [(wake, ResumePlayer pid next)])
          in case decision of
            Reply value -> schedule now (resume value)
            ResumeAt target -> schedule target (resume ())
            WhenResolved -> schedule (closesAt (engineInfo updated))
              (awaitSettlement >>= resume)
