{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module TradingGame.Interaction
  ( PlayerInteraction(..), PlayerCommand(..), PlayerInfo(..), InteractionExit(..)
  , readInput, sendInfo, execute, interactivePlayer
  ) where

import Control.Effect (Eff, Effect, (:<), send)
import Data.Time.Clock (NominalDiffTime)
import TradingGame.Concurrent
import TradingGame.Core

-- Commands and replies are transport-independent. Only the handler decides
-- how to obtain commands and deliver information (terminal, script, web, ...).
data PlayerCommand
  = Quit
  | LeaveGame
  | ShowPrivateNumber
  | ShowExchange
  | PlaceOrder LimitOrder
  | WaitFor NominalDiffTime
  | ShowSettlement
  | Help
  deriving (Eq, Show)

data PlayerInfo
  = PrivateNumber Integer
  | ExchangeSnapshot ExchangeState
  | OrderSubmitted OrderResult
  | PlayerSettlement Settlement
  | HelpInfo
  deriving (Eq, Show)

data PlayerInteraction :: Effect where
  ReadInput :: PlayerInteraction m PlayerCommand
  SendInfo :: PlayerInfo -> PlayerInteraction m ()

readInput :: PlayerInteraction :< effs => Eff effs PlayerCommand
readInput = send ReadInput

sendInfo :: PlayerInteraction :< effs => PlayerInfo -> Eff effs ()
sendInfo = send . SendInfo

execute :: (TradingGame :< effs, PlayerInteraction :< effs) => PlayerCommand -> Eff effs ()
execute command = case command of
  Quit -> pure ()
  LeaveGame -> pure ()
  ShowPrivateNumber -> getMyPrivateNumber >>= sendInfo . PrivateNumber
  ShowExchange -> getExchangeState >>= sendInfo . ExchangeSnapshot
  PlaceOrder order -> submitOrder order >>= sendInfo . OrderSubmitted
  WaitFor seconds -> wait seconds
  ShowSettlement -> awaitSettlement >>= sendInfo . PlayerSettlement
  Help -> sendInfo HelpInfo

data InteractionExit = QuitApplication | LeftGame deriving (Eq, Show)

-- Both adapters run this exact program. The update worker is scoped to the
-- command loop, and stops naturally after publishing the final settlement.
interactivePlayer
  :: (TradingGame :< effs, PlayerInteraction :< effs, Concurrent :< effs)
  => Eff effs InteractionExit
interactivePlayer = withWorkers [getExchangeState >>= updates] loop
  where
    updates snapshot = do
      sendInfo (ExchangeSnapshot snapshot)
      case gamePhase snapshot of
        Resolved _ -> awaitSettlement >>= sendInfo . PlayerSettlement
        Trading -> awaitExchangeChange snapshot >>= updates
    loop = do
      command <- readInput
      case command of
        Quit -> pure QuitApplication
        LeaveGame -> pure LeftGame
        cmd -> execute cmd >> loop
