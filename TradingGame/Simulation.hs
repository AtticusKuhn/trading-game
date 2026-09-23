{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module TradingGame.Simulation where

import Control.Effect (Eff, lift, send)
import qualified Control.Effect.State.Strict as State
import TradingGame.Concurrent (Concurrent)
import Data.Set (Set)
import Data.List (sortOn, partition)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), NominalDiffTime)
import TradingGame.Core
import TradingGame.Player

-- Scope IDs are scheduler identities, never game participants.
type ScopeId = Integer
data Activity effs = Activity PlayerId [ScopeId] (Task effs)
data Event effs = CloseExchange | ResumeTask (Activity effs)
data Subscription effs = Subscription ExchangeState (ExchangeState -> Activity effs)

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
simulatePlayers = simulatePlayersWithInstruments allInstruments

runTradingGameWithInstruments :: Set Instrument -> UTCTime -> NominalDiffTime -> [PlayerProgram effs] -> Eff effs [Settlement]
runTradingGameWithInstruments enabled start duration =
  fmap engineSettlements . simulatePlayersWithInstruments enabled start duration

simulatePlayersWithInstruments :: Set Instrument -> UTCTime -> NominalDiffTime -> [PlayerProgram effs] -> Eff effs Engine
simulatePlayersWithInstruments enabled start duration programs =
  let initial = newEngineWithInstruments enabled start duration (map fst programs)
      -- Stable sorting keeps closure ahead of wake-ups at the deadline.
      events = (closesAt (engineInfo initial), CloseExchange) :
        [(start, ResumeTask (Activity (playerID player) [] (preparePlayer program)))
        | (player, program) <- programs]
  in fst <$> simulate True start initial 0 [] (sortOn fst events)

-- Run one session against an existing engine, pausing virtual time when it
-- returns. This lets the terminal log out/rejoin without resetting the game or
-- forcing closure. Workers are still scheduled by the same deterministic queue.
runSimulatedPlayer
  :: UTCTime -> Engine -> PlayerId -> Eff (TradingGame ': Concurrent ': effs) a
  -> Eff effs (UTCTime, Engine, a)
runSimulatedPlayer now engine pid program = do
  (result, (final, stoppedAt)) <- State.runState Nothing $
    let task = preparePlayer (lift program >>= State.put . Just)
        events = [(max now (closesAt (engineInfo engine)), CloseExchange),
                  (now, ResumeTask (Activity pid [] task))]
    in simulate False now engine 0 [] (sortOn fst events)
  case result of
    Just value -> pure (stoppedAt, final, value)
    Nothing -> error "runSimulatedPlayer: player exited without returning"

-- Every trading request yields to tasks already runnable at the same time.
-- Scope bookkeeping does not consume a turn. Nothing preempts a computation
-- before a boundary; infinite immediate requests can prevent time advancing.
simulate :: Bool -> UTCTime -> Engine -> ScopeId -> [Subscription effs] -> [(UTCTime, Event effs)] -> Eff effs (Engine, UTCTime)
simulate closeWhenIdle now engine _ [] events
  | not closeWhenIdle && all onlyClosure events = pure (engine, now)
  where
    onlyClosure (_, CloseExchange) = True
    onlyClosure _ = False
simulate _ now engine _ [] [] = pure (engine, now)
simulate _ _ _ _ (_:_) [] = error "runTradingGame: all remaining tasks await exchange changes after closure"
simulate closeWhenIdle _ engine nextScope subscriptions ((now, event) : pending) =
  let current = advanceTo now engine
      continue updated fresh waiting events =
        let snapshot = exchangeSnapshot now updated
            (ready, blocked) = partition (\(Subscription previous _) -> exchangeChanged previous snapshot) waiting
            awakened = [(now, ResumeTask (resume snapshot)) | Subscription _ resume <- ready]
        in simulate closeWhenIdle now updated fresh blocked (sortOn fst (events ++ awakened))
      runTask fresh waiting events (Activity pid scopes program) = do
        step <- stepPlayer program
        let activity = Activity pid scopes
            schedule updated target next = continue updated fresh waiting
              (events ++ [(target, ResumeTask (activity next))])
        case step of
          Finished -> continue current fresh waiting events
          Requested request resume -> case request of
            StopTask -> continue current fresh waiting events
            EnterScope -> runTask (fresh + 1) waiting events (Activity pid (fresh:scopes) (resume ()))
            ForkTask -> runTask fresh waiting
              (events ++ [(now, ResumeTask (activity (resume True)))]) (activity (resume False))
            ExitScope -> case scopes of
              [] -> error "runTradingGame: unbalanced worker scope"
              scope:outer ->
                let survives (Activity _ ancestry _) = scope `notElem` ancestry
                    keepEvent (_, CloseExchange) = True
                    keepEvent (_, ResumeTask task) = survives task
                    keepSubscription (Subscription previous k) = survives (k previous)
                in runTask fresh (filter keepSubscription waiting) (filter keepEvent events)
                     (Activity pid outer (resume ()))
            TradingRequest trading ->
              let (updated, decision) = handleRequest now pid trading current
              in case decision of
                Reply value -> schedule updated now (resume value)
                ResumeAt target -> schedule updated target (resume ())
                WhenResolved -> schedule updated (closesAt (engineInfo updated))
                  (send (TradingRequest AwaitSettlement) >>= resume)
                WhenExchangeChanges previous -> continue updated fresh
                  (waiting ++ [Subscription previous (activity . resume)]) events
  in case event of
    CloseExchange -> continue current nextScope subscriptions pending
    ResumeTask task -> runTask nextScope subscriptions pending task
