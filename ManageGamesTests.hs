{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module ManageGamesTests (manageGamesProperties) where

import Control.Concurrent.Async (mapConcurrently, withAsync, wait)
import Control.Concurrent.MVar (readMVar)
import Control.Concurrent.STM
import Control.Effect (Eff, IOE, runIO)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (sortOn)
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock (addUTCTime, diffUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import LiveTests (liveProperty, manualClock)
import Network.HTTP.Types
import Network.Wai (responseToStream)
import Network.Wai.Test
import Test.QuickCheck
import TestSupport (genRoster, genOrder)
import TradingGame hiding (wait)
import TradingGame.Web
import WebTests (post, orderFields)

manage :: GameManager -> Eff '[ManageGames, IOE] a -> IO a
manage manager = runIO . runManageGames manager

genConfig :: Gen NewGameConfig
genConfig = do
  enabled <- Set.fromList <$> sublistOf [minBound .. maxBound]
  roster <- genRoster
  types <- vectorOf (length roster) (elements [HumanPlayer, RandomTradingBot, MarketMakingBot])
  offset <- arbitrary
  Positive duration <- arbitrary
  let start = addUTCTime (fromInteger offset) simulationStart
  pure (NewGameConfig start (addUTCTime (fromInteger duration) start)
    (zipWith (RosterEntry . displayName) roster types) enabled)

shrinkConfig :: NewGameConfig -> [NewGameConfig]
shrinkConfig config =
  [config { gameRoster = roster } | roster <- shrinkList (const []) (gameRoster config), not (null roster)]
  ++ [config { gameEnd = addUTCTime (fromInteger duration) (gameStart config) }
     | duration <- shrink (round (diffUTCTime (gameEnd config) (gameStart config)) :: Integer), duration > 0]

humans :: NewGameConfig -> NewGameConfig
humans config = config { gameRoster = map (\entry -> entry { rosterType = HumanPlayer }) (gameRoster config) }

-- Creation/list/lookup form a map, including simultaneous duplicate configurations.
prop_directory :: Property
prop_directory = forAllShrink (listOf genConfig) (shrinkList shrinkConfig) $ \configs -> liveProperty "game directory" $ do
  (clock, _) <- manualClock simulationStart
  withGameManager clock $ \manager -> do
    results <- mapConcurrently (manage manager . createNewGame) configs
    let ids = [gid | Right gid <- results]
    listed <- manage manager listAllGames
    found <- mapM (manage manager . lookupGame) ids
    pure $ conjoin
      [ length ids === length configs
      , map summaryId listed === sortOn id ids
      , map (fmap summaryConfig) found === map Just configs
      ]

-- Arbitrary windows preserve their original endpoints; only due games expose
-- a runtime, and server-generated secrets and accounts cover the fixed roster.
prop_window :: Property
prop_window = forAllShrink (humans <$> genConfig) shrinkConfig $ \config -> liveProperty "game window" $ do
  (clock, _) <- manualClock simulationStart
  withGameManager clock $ \manager -> do
    Right gid <- manage manager (createNewGame config)
    Just summary <- manage manager (lookupGame gid)
    active <- gameRuntime manager gid
    let expected | simulationStart < gameStart config = Upcoming
                 | simulationStart < gameEnd config = Running
                 | otherwise = Completed
    engineChecks <- case active of
      Nothing -> pure (property True)
      Just runtime -> do
        engine <- readMVar (runtimeEngine runtime)
        pure $ conjoin
          [ opensAt (engineInfo engine) === gameStart config
          , closesAt (engineInfo engine) === gameEnd config
          , enabledInstruments (engineInfo engine) === gameInstruments config
          , Map.keysSet (books (engineBook engine)) === gameInstruments config
          , map displayName (players engine) === map rosterName (gameRoster config)
          , Map.keysSet (accounts (engineBook engine)) === Set.fromList (map playerID (players engine))
          , property (all (\p -> privateNumber p >= 1 && privateNumber p <= 9) (players engine))
          , enginePhase engine === if expected == Completed then Resolved (engineResolutions engine) else Trading
          ]
    pure $ conjoin [summaryStatus summary === expected, isJust active === (expected /= Upcoming), engineChecks]

-- No request drives these transitions: the manager starts and settles idle
-- games (and their bots) when the injected clock crosses each boundary.
prop_automaticLifecycle :: Property
prop_automaticLifecycle = forAllShrink genConfig shrinkConfig $ \generated ->
  forAll (arbitrary :: Gen (Positive Integer)) $ \(Positive lead) -> liveProperty "scheduled lifecycle" $ do
    let config = generated { gameStart = addUTCTime (fromInteger lead) simulationStart
                           , gameEnd = addUTCTime (fromInteger lead + diffUTCTime (gameEnd generated) (gameStart generated)) simulationStart }
    (clock, advance) <- manualClock simulationStart
    withGameManager clock $ \manager -> do
      Right gid <- manage manager (createNewGame config)
      revision <- readTVarIO (gameRevision manager)
      before <- gameRuntime manager gid
      advance (gameStart config)
      atomically (readTVar (gameRevision manager) >>= check . (> revision))
      Just runtime <- gameRuntime manager gid
      initial <- readMVar (runtimeEngine runtime)
      advance (gameEnd config)
      final <- atomically (readTMVar (runtimeFinal runtime))
      pure $ conjoin
        [ property (isNothing before)
        , players final === players initial
        , enginePhase final === Resolved (engineResolutions initial)
        , Map.keysSet (accounts (engineBook final)) === Set.fromList (map playerID (players initial))
        ]

-- Mutating one exchange cannot affect another, even with identical names/IDs.
prop_isolation :: Property
prop_isolation = forAllShrink (humans <$> genConfig) shrinkConfig $ \config ->
  forAll (listOf genOrder) $ \orders -> liveProperty "game isolation" $ do
    (clock, _) <- manualClock (gameStart config)
    withGameManager clock $ \manager -> do
      Right first <- manage manager (createNewGame config)
      Right second <- manage manager (createNewGame config)
      Just a <- gameRuntime manager first
      Just b <- gameRuntime manager second
      before <- readMVar (runtimeEngine b)
      initial <- readMVar (runtimeEngine a)
      let pid = playerID (head (players initial))
      mapM_ (requestLive a pid . SubmitOrder) orders
      after <- readMVar (runtimeEngine b)
      actual <- readMVar (runtimeEngine a)
      let expected = foldl (\engine order -> fst (handleRequest (gameStart config) pid (SubmitOrder order) engine)) initial orders
      pure $ conjoin [after === before, actual === expected]

prop_invalidConfig :: Property
prop_invalidConfig = forAllShrink genConfig shrinkConfig $ \config ->
  forAll (elements [ config { gameEnd = gameStart config }, config { gameRoster = [] }
                  , config { gameRoster = gameRoster config ++ gameRoster config }
                  , config { gameRoster = [RosterEntry " " HumanPlayer] } ]) $ \invalid ->
    liveProperty "invalid game creation" $ do
      (clock, _) <- manualClock simulationStart
      withGameManager clock $ \manager -> do
        result <- manage manager (createNewGame invalid)
        listed <- manage manager listAllGames
        pure $ conjoin [property (case result of Left _ -> True; Right _ -> False), listed === []]

configFields :: NewGameConfig -> [(B.ByteString, B.ByteString)]
configFields config = [("start", stamp (gameStart config)), ("end", stamp (gameEnd config))]
  ++ [("instruments-present", "yes")]
  ++ [("instrument", B.pack (show asset)) | asset <- Set.toAscList (gameInstruments config)]
  ++ concat [[("player-name", T.encodeUtf8 (T.pack (rosterName entry))), ("player-type", kind (rosterType entry))]
            | entry <- gameRoster config]
  where
    stamp = B.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"
    kind HumanPlayer = "human"
    kind RandomTradingBot = "random"
    kind MarketMakingBot = "maker"

prop_creationForm :: Property
prop_creationForm = forAllShrink genConfig shrinkConfig $ \config ->
  parseNewGameForm (configFields config) === Right config

-- A token valid for one game never authorizes an order in any other game.
prop_httpIsolation :: Property
prop_httpIsolation = forAllShrink (humans <$> genConfig) shrinkConfig $ \config -> forAll genOrder $ \order ->
  liveProperty "HTTP game isolation" $ do
    (clock, _) <- manualClock (gameStart config)
    withGameManager clock $ \manager -> do
      lobby <- newWebLobby manager 3600
      Right first <- manage manager (createNewGame config)
      Right second <- manage manager (createNewGame config)
      let path gid suffix = T.encodeUtf8 (gamePath gid <> suffix)
      joined <- runSession (srequest (post (path first "/join") []
        [("name", T.encodeUtf8 (T.pack (rosterName (head (gameRoster config)))))])) (lobbyApplication lobby)
      let cookie = maybe "" (B.takeWhile (/= ';')) (lookup "Set-Cookie" (simpleHeaders joined))
      responses <- mapM (\gid -> runSession (srequest (post (path gid "/orders") [(hCookie, cookie)] (orderFields order)))
        (lobbyApplication lobby)) [first, second]
      pure $ map simpleStatus responses === [status200, status401]

-- Browser identity selection is restricted to the human part of each roster.
prop_playerTypes :: Property
prop_playerTypes = forAllShrink genConfig shrinkConfig $ \config ->
  forAll (elements (gameRoster config)) $ \entry -> liveProperty "human roster selection" $ do
    (clock, _) <- manualClock (gameStart config)
    withGameManager clock $ \manager -> do
      lobby <- newWebLobby manager 3600
      Right gid <- manage manager (createNewGame config)
      joined <- runSession (srequest (post (T.encodeUtf8 (gamePath gid <> "/join")) []
        [("name", T.encodeUtf8 (T.pack (rosterName entry)))])) (lobbyApplication lobby)
      pure $ simpleStatus joined === if rosterType entry == HumanPlayer then status303 else status400

-- Before opening, even hand-crafted join/order/event requests are gated at
-- the directory, and public pages contain exactly the configured roster names.
prop_futureHTTP :: Property
prop_futureHTTP = forAllShrink genConfig shrinkConfig $ \config ->
  forAll (arbitrary :: Gen (Positive Integer)) $ \(Positive lead) -> liveProperty "future HTTP gates" $ do
    (clock, _) <- manualClock (addUTCTime (negate (fromInteger lead)) (gameStart config))
    withGameManager clock $ \manager -> do
      lobby <- newWebLobby manager 3600
      created <- runSession (srequest (post "/games" [] (configFields config))) (lobbyApplication lobby)
      [summary] <- manage manager listAllGames
      let base = T.encodeUtf8 (gamePath (summaryId summary))
      responses <- mapM (\endpoint -> runSession (srequest (post (base <> endpoint) [] [])) (lobbyApplication lobby)) ["/join", "/orders"]
      stream <- runSession (request (setPath defaultRequest (base <> "/events"))) (lobbyApplication lobby)
      active <- gameRuntime manager (summaryId summary)
      pure $ conjoin
        [ simpleStatus created === status303
        , map simpleStatus (stream:responses) === replicate (length responses + 1) status409
        , property (isNothing active)
        , summaryConfig summary === config
        ]

-- The upcoming page's SSE stream opens joining automatically at the start.
prop_startStream :: Property
prop_startStream = forAllShrink genConfig shrinkConfig $ \config ->
  forAll (arbitrary :: Gen (Positive Integer)) $ \(Positive lead) -> liveProperty "SSE game start" $ do
    (clock, advance) <- manualClock (addUTCTime (negate (fromInteger lead)) (gameStart config))
    withGameManager clock $ \manager -> do
      Right gid <- manage manager (createNewGame config)
      chunks <- newTVarIO []
      let snapshot = do
            Just summary <- manage manager (lookupGame gid)
            pure ("availability", availabilityView summary, summaryStatus summary /= Upcoming)
          (_, _, stream) = responseToStream (directoryStream manager snapshot)
          write chunk = atomically (modifyTVar' chunks (++ [toLazyByteString chunk]))
      withAsync (stream (\body -> body write (pure ()))) $ \connection -> do
        atomically (readTVar chunks >>= check . not . null)
        advance (gameStart config)
        wait connection
        wire <- LBS.concat <$> readTVarIO chunks
        pure $ property ("event: closed\ndata: done\n\n" `LBS.isSuffixOf` wire)

manageGamesProperties :: [Property]
manageGamesProperties = [prop_directory, prop_window, prop_automaticLifecycle, prop_isolation,
  prop_invalidConfig, prop_creationForm, prop_defaultInstruments, prop_httpIsolation, prop_playerTypes, prop_futureHTTP, prop_startStream]

-- An omitted selection uses the default; an explicit empty selection stays empty.
prop_defaultInstruments :: Property
prop_defaultInstruments = forAll genConfig $ \config ->
  let fields = filter (\(key, _) -> key /= "instruments-present" && key /= "instrument") (configFields config)
  in conjoin
    [ fmap gameInstruments (parseNewGameForm fields) === Right allInstruments
    , fmap gameInstruments (parseNewGameForm (("instruments-present", "yes") : fields)) === Right Set.empty
    ]
