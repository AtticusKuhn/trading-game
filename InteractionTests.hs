{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module InteractionTests (interactionProperties) where

import qualified Control.Concurrent.Async as Async
import Control.Concurrent.MVar
import Control.Effect (Eff, interpret, lift, liftIO, run, runIO)
import qualified Control.Effect.State.Strict as State
import Data.List (foldl')
import Data.Ratio ((%))
import Data.Time.Clock (addUTCTime)
import Test.QuickCheck
import LiveTests (manualClock, liveProperty)
import TestSupport (genOrder)
import TradingGame
import TradingGame.Terminal (parseCommand)

type ScriptState = ([PlayerCommand], [PlayerInfo])

-- An entirely pure handler, outside the simulator/worker. The remaining input
-- lets properties detect accidental reads after Quit as well as lost replies.
runScript
  :: [PlayerCommand]
  -> Eff (PlayerInteraction ': effs) a
  -> Eff effs (ScriptState, a)
runScript commands action = State.runState (commands, []) $
  interpret (\request -> case request of
    ReadInput -> do
      (remaining, output) <- State.get @ScriptState
      case remaining of
        [] -> error "script unexpectedly exhausted"
        command:rest -> State.put (rest, output) >> pure command
    SendInfo info -> State.modify @ScriptState (\(remaining, output) -> (remaining, output ++ [info])))
    (lift action)

interactionProperties :: [Property]
interactionProperties =
  [ property prop_simulationInteraction
  , property prop_liveWorkerInteraction
  , property prop_liveScopeInteraction
  , property prop_parsePrices
  , once prop_invalidCommands
  ]

prop_simulationInteraction :: Int -> Positive Integer -> Property
prop_simulationInteraction secret (Positive delay) =
  forAll (listOf genOrder) $ \orders ->
    let pid = PlayerId 42
        duration = fromRational (delay % 2)
        initial = newEngine simulationStart duration [(pid, toInteger secret)]
        ordered = foldl' (\engine order -> fst (handleRequest simulationStart pid (SubmitOrder order) engine)) initial orders
        snapshot now engine = case snd (handleRequest now pid GetExchangeState engine) of
          Reply value -> ExchangeSnapshot value
        later = addUTCTime (fromInteger delay) simulationStart
        result = Settlement (toInteger secret) 0
        unread = [Help]
        commands = [ShowPrivateNumber] ++ map PlaceOrder orders ++
          [ShowExchange, WaitFor (fromInteger delay), ShowExchange, ShowSettlement,
           ShowPrivateNumber, Quit] ++ unread
        expected = [PrivateNumber (toInteger secret)] ++
          map (OrderSubmitted . Right . OrderId) [1 .. toInteger (length orders)] ++
          [snapshot simulationStart ordered, snapshot later ordered,
           PlayerSettlement result, PrivateNumber (toInteger secret)]
        ((remaining, output), settlements) = run $ runScript commands $
          runTradingGameFor duration [(pid, interactivePlayer, secret)]
    in conjoin [remaining === unread, output === expected, settlements === [result]]

-- The individual worker handles only TradingGame, without Concurrent or a
-- handler inside the worker. Compare its replies with virtual execution.
prop_liveWorkerInteraction :: Int -> Property
prop_liveWorkerInteraction secret = forAll (listOf genOrder) $ \orders ->
  liveProperty "interaction forwarded by individual live worker" $ do
    (clock, _) <- manualClock simulationStart
    let initial = newEngine simulationStart 60 [(PlayerId 42, toInteger secret)]
        commands = [ShowPrivateNumber] ++ map PlaceOrder orders ++ [ShowExchange, Quit, Help]
        expected = fst $ run $ runScript commands $
          runTradingGameFor 60 [(PlayerId 42, interactivePlayer, secret)]
    runtime <- newLiveRuntime clock (const (pure ())) initial
    (actual, ()) <- runIO $ runScript commands $
      runLivePlayer (closesAt (engineInfo initial)) runtime (PlayerId 42, interactivePlayer, secret)
    pure (actual === expected)

-- Handle PlayerInteraction around the whole concurrent run. Synchronization
-- ensures replies have been delivered before closure cancels player workers.
prop_liveScopeInteraction :: Int -> Property
prop_liveScopeInteraction secret = liveProperty "outer interaction handler in live scope" $ do
  (clock, advance) <- manualClock simulationStart
  delivered <- newEmptyMVar
  let player = interactivePlayer >> liftIO (putMVar delivered ())
      config = defaultLiveConfig { liveDuration = 60 }
      action = runIO $ runScript [ShowPrivateNumber, Quit, Help] $ runConcurrent $
        runLiveWith clock config [(PlayerId 42, player, secret)]
  Async.withAsync action $ \game -> do
    takeMVar delivered
    advance (addUTCTime 60 simulationStart)
    result <- Async.wait game
    pure (result === (([Help], [PrivateNumber (toInteger secret)]), [Settlement (toInteger secret) 0]))

prop_parsePrices :: Integer -> Positive Integer -> Positive Integer -> Property
prop_parsePrices n (Positive d) (Positive quantity) =
  let command = "buy " ++ show n ++ "/" ++ show d ++ " " ++ show quantity
  in parseCommand command === Right (PlaceOrder (LimitOrder Buy (Price (n % d)) quantity))

prop_invalidCommands :: Property
prop_invalidCommands = conjoin
  [ counterexample command $ property $ case parseCommand command of
      Left _ -> True
      Right _ -> False
  | command <- ["", "buy 1/0 1", "buy NaN 1", "sell Infinity 1", "buy 2 0",
                "sell 2 -1", "buy 2 1.5", "wait -1", "wait 1/0", "quit extra"]
  ] .&&. (parseCommand "sell -1.25 2" === Right (PlaceOrder (LimitOrder Sell (Price ((-5) % 4)) 2)))
