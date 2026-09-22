{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module TradingGame.Session
  ( PlayerSession(..), SessionError(..), LoginResult, LogoutResult
  , joinGameAsPlayer, logout, getCurrentPlayer, runPlayerSession
  ) where

import Control.Effect (Eff, Effect, (:<), interpret, lift, send)
import qualified Control.Effect.State.Strict as State
import Data.List (find)
import TradingGame.Core

data SessionError
  = NotLoggedIn
  | UnknownPlayerName String
  | UnknownPlayerId PlayerId
  | AlreadyLoggedIn PlayerId
  deriving (Eq, Show)

type LoginResult = Either SessionError PlayerId
type LogoutResult = Either SessionError ()

data PlayerSession :: Effect where
  JoinGameAsPlayer :: String -> PlayerSession m LoginResult
  Logout :: PlayerSession m LogoutResult
  GetCurrentPlayer :: PlayerSession m (Maybe PlayerId)

joinGameAsPlayer :: PlayerSession :< effs => String -> Eff effs LoginResult
joinGameAsPlayer = send . JoinGameAsPlayer

logout :: PlayerSession :< effs => Eff effs LogoutResult
logout = send Logout

getCurrentPlayer :: PlayerSession :< effs => Eff effs (Maybe PlayerId)
getCurrentPlayer = send GetCurrentPlayer

-- One handler per terminal/connection. The host supplies the engine's fixed
-- roster; login changes only this session, never player records or accounts.
-- Names match exactly. Repeating a login is idempotent; switching requires logout.
runPlayerSession :: [Player] -> Eff (PlayerSession ': effs) a -> Eff effs a
runPlayerSession roster action = fmap snd $ State.runState (Nothing :: Maybe PlayerId) $
  interpret (\request -> case request of
    GetCurrentPlayer -> State.get @(Maybe PlayerId)
    Logout -> do
      current <- State.get @(Maybe PlayerId)
      State.put (Nothing :: Maybe PlayerId)
      pure $ maybe (Left NotLoggedIn) (const (Right ())) current
    JoinGameAsPlayer name -> case find ((== name) . displayName) roster of
      Nothing -> pure (Left (UnknownPlayerName name))
      Just player -> do
        current <- State.get @(Maybe PlayerId)
        case current of
          Just pid | pid /= playerID player -> pure (Left (AlreadyLoggedIn pid))
          _ -> do
            State.put (Just (playerID player))
            pure (Right (playerID player)))
    (lift action)
