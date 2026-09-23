{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module InteractionTests (interactionProperties) where

import Control.Concurrent.STM
import Control.Effect (Eff, interpret, lift, liftIO, run, runIO)
import qualified Control.Effect.State.Strict as State
import Control.Monad (void)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Ratio ((%))
import Data.List (nub)
import Test.QuickCheck
import LiveTests (manualClock, liveProperty)
import TestSupport (genOrder, genRoster)
import TradingGame
import TradingGame.Terminal (parseCommand)

type ScriptState = ([PlayerCommand], [PlayerInfo])

runScript :: [PlayerCommand] -> Eff (PlayerInteraction ': effs) a -> Eff effs (ScriptState, a)
runScript commands action = State.runState (commands, []) $
  interpret (\request -> case request of
    ReadInput -> do
      (remaining, output) <- State.get @ScriptState
      case remaining of
        [] -> error "script unexpectedly exhausted"
        command:rest -> State.put (rest, output) >> pure command
    SendInfo info -> State.modify @ScriptState (\(remaining, output) -> (remaining, output ++ [info])))
    (lift action)

-- Unsolicited public updates can interleave with command replies in live time.
commandReplies :: [PlayerInfo] -> [PlayerInfo]
commandReplies = filter (\info -> case info of
  ExchangeSnapshot _ -> False
  PlayerSettlement _ -> False
  _ -> True)

genCommands :: Gen [PlayerCommand]
genCommands = listOf (oneof [PlaceOrder <$> genOrder, elements [ShowPrivateNumber, Help]])

interactionProperties :: [Property]
interactionProperties =
  [ property prop_exit
  , property prop_automaticSettlement
  , property prop_liveSimulationReplies
  , property prop_parsePrices
  , once prop_invalidCommands
  ]

-- Quit/logout return distinct outcomes and leave arbitrary trailing input unread.
prop_exit :: Bool -> [Bool] -> Positive Integer -> Property
prop_exit quitting trailing (Positive duration) = forAll genRoster $ \roster ->
  forAll genCommands $ \commands ->
    let stop = if quitting then Quit else LeaveGame
        expected = if quitting then QuitApplication else LeftGame
        suffix = map (\b -> if b then Help else Quit) trailing
        program = interactivePlayer >>= State.put . Just
        (outcome, ((remaining, _), _)) = run $ State.runState Nothing $
          runScript (commands ++ [stop] ++ suffix) $
            runTradingGameFor (fromInteger duration) [(head roster, program)]
    in conjoin [remaining === suffix, outcome === Just expected]

-- Updates and settlement are delivered while the command loop is waiting.
prop_automaticSettlement :: Positive Integer -> Positive Integer -> Property
prop_automaticSettlement (Positive duration) (Positive later) = forAll genRoster $ \roster ->
  let player = head roster
      commands = [WaitFor (fromInteger (duration + later)), Quit]
      ((_, output), settlements) = run $ runScript commands $
        runTradingGameFor (fromInteger duration) [(player, void interactivePlayer)]
  in conjoin
    [ [value | PlayerSettlement value <- output] === settlements
    , nub [gamePhase value | ExchangeSnapshot value <- output] === [Trading, Resolved (Map.fromSet (\asset -> resolve asset [privateNumber player]) allInstruments)]
    ]

-- The same player and input produce the same command replies under pure
-- scheduling and real threads. The live transport uses STM for shared output.
prop_liveSimulationReplies :: Positive Integer -> Property
prop_liveSimulationReplies (Positive duration) = forAll genRoster $ \roster ->
  forAll (Set.fromList <$> sublistOf [minBound .. maxBound]) $ \enabled ->
  forAll genCommands $ \commands -> liveProperty "shared interactive player replies" $ do
    let player = head roster
        input = commands ++ [Quit]
        initial = newEngineWithInstruments enabled simulationStart (fromInteger duration) [player]
        ((_, expected), _) = run $ runScript input $
          runTradingGameWithInstruments enabled simulationStart (fromInteger duration) [(player, void interactivePlayer)]
    (clock, _) <- manualClock simulationStart
    runtime <- newLiveRuntime clock (const (pure ())) initial
    transport <- newTVarIO (input, [])
    runIO $ interpret (\request -> liftIO $ atomically $ case request of
      ReadInput -> do
        (remaining, output) <- readTVar transport
        case remaining of
          [] -> error "script unexpectedly exhausted"
          command:rest -> writeTVar transport (rest, output) >> pure command
      SendInfo info -> modifyTVar' transport (\(remaining, output) -> (remaining, output ++ [info]))) $
      runLivePlayer runtime (player, void interactivePlayer)
    (remaining, actual) <- readTVarIO transport
    pure (conjoin [remaining === [], commandReplies actual === commandReplies expected])

prop_parsePrices :: Integer -> Positive Integer -> Positive Integer -> Property
prop_parsePrices n (Positive d) (Positive quantity) =
  forAll (elements [minBound .. maxBound]) $ \asset ->
    let command = "buy " ++ show asset ++ " " ++ show n ++ "/" ++ show d ++ " " ++ show quantity
    in parseCommand command === Right (PlaceOrder (LimitOrder Buy (Price (n % d)) asset quantity))

prop_invalidCommands :: Property
prop_invalidCommands = conjoin
  [ counterexample command $ property $ case parseCommand command of
      Left _ -> True
      Right _ -> False
  | command <- ["", "buy 1/0 1", "buy NaN 1", "sell Infinity 1", "buy 2 0",
                "sell 2 -1", "buy 2 1.5", "wait -1", "wait 1/0", "quit extra"]
  ] .&&. (parseCommand "sell -1.25 2" === Right (PlaceOrder (LimitOrder Sell (Price ((-5) % 4)) Sum 2)))
