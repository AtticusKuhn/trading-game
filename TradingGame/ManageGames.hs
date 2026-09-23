{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

-- Scheduling belongs to the host. No LiveRuntime exists before a game's start.
module TradingGame.ManageGames
  ( ManageGames(..), GameId(..), PlayerType(..), RosterEntry(..)
  , NewGameConfig(..), defaultNewGameConfig, CreateGameError(..), GameStatus(..), GameSummary(..)
  , GameManager, withGameManager, runManageGames
  , createNewGame, lookupGame, listAllGames
  , gameRuntime, gameRevision
  ) where

import Control.Concurrent.Async (Async, asyncWithUnmask, cancel)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Effect (Eff, Effect, IOE, (:<), interpret, liftIO, runIO, send)
import Control.Exception (SomeException, bracket, displayException, try)
import Control.Monad (void)
import Data.Char (isSpace)
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.Time.Clock (UTCTime, diffUTCTime)
import System.Random (StdGen, newStdGen, randomRIO)
import TradingGame.Bots
import TradingGame.Concurrent
import TradingGame.Core
import TradingGame.Live

newtype GameId = GameId Integer deriving (Eq, Ord, Show)
data PlayerType = HumanPlayer | RandomTradingBot | MarketMakingBot deriving (Eq, Show)
data RosterEntry = RosterEntry
  { rosterName :: String
  , rosterType :: PlayerType
  } deriving (Eq, Show)
data NewGameConfig = NewGameConfig
  { gameStart :: UTCTime
  , gameEnd :: UTCTime
  , gameRoster :: [RosterEntry]
  , gameInstruments :: Set Instrument
  } deriving (Eq, Show)

-- Hosts can override the instrument set before submitting the configuration.
defaultNewGameConfig :: UTCTime -> UTCTime -> [RosterEntry] -> NewGameConfig
defaultNewGameConfig start end roster = NewGameConfig start end roster allInstruments

data CreateGameError = InvalidTimeWindow | EmptyRoster | BlankPlayerName
  | DuplicatePlayerNames | ManagerClosed deriving (Eq, Show)

-- These are directory statuses, not phases of the exchange model.
data GameStatus = Upcoming | Running | Completed | Failed deriving (Eq, Show)
data GameSummary = GameSummary
  { summaryId :: GameId
  , summaryConfig :: NewGameConfig
  , summaryStatus :: GameStatus
  } deriving (Eq, Show)

data ManageGames :: Effect where
  CreateNewGame :: NewGameConfig -> ManageGames m (Either CreateGameError GameId)
  LookupGame :: GameId -> ManageGames m (Maybe GameSummary)
  ListAllGames :: ManageGames m [GameSummary]

createNewGame :: ManageGames :< effs => NewGameConfig -> Eff effs (Either CreateGameError GameId)
createNewGame = send . CreateNewGame
lookupGame :: ManageGames :< effs => GameId -> Eff effs (Maybe GameSummary)
lookupGame = send . LookupGame
listAllGames :: ManageGames :< effs => Eff effs [GameSummary]
listAllGames = send ListAllGames

data ManagedGame = ManagedGame
  { managedConfig :: NewGameConfig
  , managedInitial :: Engine
  , managedSeeds :: [StdGen]
  , managedRuntime :: MVar (Maybe LiveRuntime)
  , managedFailure :: TVar (Maybe String)
  }
data Registry = Registry
  { registryNext :: Integer
  , registryGames :: Map.Map GameId (ManagedGame, Async ())
  , registryClosed :: Bool
  }
data GameManager = GameManager
  { managerClock :: LiveClock
  , managerRegistry :: MVar Registry
  , gameRevision :: TVar Integer
  }

-- Shutdown cancels and joins both waiting games and active bot/deadline scopes.
withGameManager :: LiveClock -> (GameManager -> IO a) -> IO a
withGameManager clock = bracket acquire release
  where
    acquire = GameManager clock <$> newMVar (Registry 1 Map.empty False) <*> newTVarIO 0
    release manager = do
      workers <- modifyMVar (managerRegistry manager) $ \registry ->
        pure (registry { registryClosed = True }, map snd (Map.elems (registryGames registry)))
      mapM_ cancel workers

runManageGames :: IOE :< effs => GameManager -> Eff (ManageGames ': effs) a -> Eff effs a
runManageGames manager = interpret $ \request -> liftIO $ case request of
  CreateNewGame config -> createGameIO manager config
  LookupGame gid -> do
    games <- readMVar (managerRegistry manager)
    traverse (summarize manager gid . fst) (Map.lookup gid (registryGames games))
  ListAllGames -> do
    games <- readMVar (managerRegistry manager)
    mapM (\(gid, (game, _)) -> summarize manager gid game) (Map.toAscList (registryGames games))

validate :: NewGameConfig -> Either CreateGameError ()
validate config
  | gameEnd config <= gameStart config = Left InvalidTimeWindow
  | null names = Left EmptyRoster
  | any (all isSpace) names = Left BlankPlayerName
  | length (nub names) /= length names = Left DuplicatePlayerNames
  | otherwise = Right ()
  where names = map rosterName (gameRoster config)

createGameIO :: GameManager -> NewGameConfig -> IO (Either CreateGameError GameId)
createGameIO manager config = case validate config of
  Left problem -> pure (Left problem)
  Right () -> modifyMVarMasked (managerRegistry manager) $ \registry ->
    if registryClosed registry then pure (registry, Left ManagerClosed) else do
      roster <- sequence [Player (PlayerId n) (rosterName entry) <$> randomRIO (1, 9)
        | (n, entry) <- zip [1..] (gameRoster config)]
      seeds <- mapM (const newStdGen) roster
      runtime <- newMVar Nothing
      failure <- newTVarIO Nothing
      let gid = GameId (registryNext registry)
          game = ManagedGame config
            (newEngineWithInstruments (gameInstruments config) (gameStart config) (diffUTCTime (gameEnd config) (gameStart config)) roster)
            seeds runtime failure
      -- Immediate games are available before creation returns, even if already expired.
      void (activate manager game)
      worker <- asyncWithUnmask $ \unmask -> do
        result <- try (unmask (executeGame manager game)) :: IO (Either SomeException ())
        case result of
          Left problem -> atomically $ do
            writeTVar failure (Just (displayException problem))
            modifyTVar' (gameRevision manager) (+1)
          Right () -> pure ()
      atomically (modifyTVar' (gameRevision manager) (+1))
      pure (registry { registryNext = registryNext registry + 1
                     , registryGames = Map.insert gid (game, worker) (registryGames registry) }, Right gid)

activate :: GameManager -> ManagedGame -> IO (Maybe LiveRuntime)
activate manager game = modifyMVar (managedRuntime game) $ \existing -> case existing of
  Just runtime -> pure (existing, Just runtime)
  Nothing -> do
    now <- clockNow (managerClock manager)
    if now < gameStart (managedConfig game) then pure (Nothing, Nothing) else do
      runtime <- newLiveRuntime (managerClock manager) (const (pure ())) (managedInitial game)
      -- Publish settlement before exposing games whose end is already past.
      void $ modifyLiveEngine runtime $ \time engine -> pure (advanceTo time engine, ())
      atomically (modifyTVar' (gameRevision manager) (+1))
      pure (Just runtime, Just runtime)

executeGame :: GameManager -> ManagedGame -> IO ()
executeGame manager game = do
  clockAlarm (managerClock manager) (gameStart (managedConfig game)) >>= atomically
  active <- activate manager game
  case active of
    Nothing -> ioError (userError "Game clock alarm fired before the start")
    Just runtime -> do
      let programs = [ (player, case rosterType entry of
                           HumanPlayer -> pure ()
                           RandomTradingBot -> randomTrader seed
                           MarketMakingBot -> marketMaker)
                     | (player, entry, seed) <- zip3 (players (managedInitial game))
                         (gameRoster (managedConfig game)) (managedSeeds game)
                     , rosterType entry /= HumanPlayer ]
      runIO $ runConcurrent $ withWorkers (map (runLivePlayer runtime) programs)
        (liftIO (void (runExchange runtime)))
      atomically (modifyTVar' (gameRevision manager) (+1))

summarize :: GameManager -> GameId -> ManagedGame -> IO GameSummary
summarize manager gid game = do
  now <- clockNow (managerClock manager)
  failure <- readTVarIO (managedFailure game)
  let config = managedConfig game
      status | Just _ <- failure = Failed
             | now < gameStart config = Upcoming
             | now < gameEnd config = Running
             | otherwise = Completed
  pure (GameSummary gid config status)

-- Host adapter access only. Scheduled games cannot expose trading or sessions.
-- Activation here also makes the exact start boundary independent of scheduling
-- latency in the background worker. The runtime is allocated at most once.
gameRuntime :: GameManager -> GameId -> IO (Maybe LiveRuntime)
gameRuntime manager gid = do
  registry <- readMVar (managerRegistry manager)
  if registryClosed registry then pure Nothing else case Map.lookup gid (registryGames registry) of
    Nothing -> pure Nothing
    Just (game, _) -> do
      failure <- readTVarIO (managedFailure game)
      case failure of
        Just _ -> pure Nothing
        Nothing -> activate manager game
