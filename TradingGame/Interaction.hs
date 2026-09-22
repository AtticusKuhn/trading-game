{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module TradingGame.Interaction
  ( PlayerInteraction(..), PlayerCommand(..), PlayerInfo(..)
  , readInput, sendInfo, execute, interactivePlayer
  ) where

import Control.Effect (Eff, Effect, (:<), send)
import Data.Time.Clock (NominalDiffTime)
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

-- In particular, this specializes to Eff '[TradingGame, PlayerInteraction] ().
interactivePlayer :: (TradingGame :< effs, PlayerInteraction :< effs) => Eff effs ()
interactivePlayer = loop
  where
    loop = do
      command <- readInput
      case command of
        Quit -> pure ()
        LeaveGame -> pure ()
        cmd -> execute cmd >> loop
