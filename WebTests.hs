{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module WebTests (webProperties, post, orderFields) where

import Control.Concurrent.Async (mapConcurrently)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.MVar (readMVar)
import Control.Concurrent.STM
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Either (isRight)
import Data.List (find, nub, sort)
import Control.Monad (void)
import Data.Time.Clock (NominalDiffTime)
import System.Random (randomRIO)
import Data.Maybe (isJust)
import Data.Ratio (denominator, numerator)
import LiveTests (liveProperty, manualClock)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Test
import Test.QuickCheck
import TestSupport (genOrder, genEngine, genRoster)
import Text.Blaze.Html5 (toHtml)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import TradingGame
import TradingGame.Web

-- Player identities are fixed; only their private numbers are drawn at startup.
webPlayerNames :: [String]
webPlayerNames =
  ["alice", "bob", "carol", "dan", "eve", "fred", "gwen", "hal", "market-maker", "noise-trader"]

newWebGame :: NominalDiffTime -> IO WebGame
newWebGame duration = do
  clock <- newLiveClock
  start <- clockNow clock
  roster <- sequence
    [Player (PlayerId n) name <$> randomRIO (1, 9) | (n, name) <- zip [1..] webPlayerNames]
  runtime <- newLiveRuntime clock (const (pure ())) (newEngine start duration roster)
  sessions <- newTVarIO Map.empty
  pure (WebGame runtime sessions roster [])

post :: B.ByteString -> RequestHeaders -> [(B.ByteString, B.ByteString)] -> SRequest
post path headers fields = SRequest
  ((setPath defaultRequest path) { requestMethod = "POST", requestHeaders = headers })
  (LBS.fromStrict (renderSimpleQuery False fields))

orderFields :: LimitOrder -> [(B.ByteString, B.ByteString)]
orderFields order =
  [("side", if orderSide order == Buy then "buy" else "sell")
  ,("instrument", B.pack (show (instrument order)))
  ,("price", B.pack (show (numerator value) ++ "/" ++ show (denominator value)))
  ,("quantity", B.pack (show (orderQuantity order)))]
  where
    -- Submit exact inputs; display formatting intentionally rounds prices.
    Price value = limitPrice order

-- Arbitrary valid orders cannot change any exchange state without a session.
prop_requiresSession :: Property
prop_requiresSession = forAll genOrder $ \order -> ioProperty $ do
  game <- newWebGame 3600
  before <- readMVar (runtimeEngine (webRuntime game))
  response <- runSession (srequest (post "/orders" [] (orderFields order))) (webApplication game)
  after <- readMVar (runtimeEngine (webRuntime game))
  pure $ conjoin [simpleStatus response === status401, after === before]

-- HTTP parsing and session binding agree with the pure engine for arbitrary
-- orders, even with a forged player ID in the form. Another account is untouched.
prop_boundOrder :: Property
prop_boundOrder = forAll genOrder $ \order -> ioProperty $ do
  game <- newWebGame 3600
  Right token <- joinWebPlayer game "alice"
  Right _ <- joinWebPlayer game "bob"
  initial <- readMVar (runtimeEngine (webRuntime game))
  let fields = ("player", "bob") : orderFields order
      headers = [(hCookie, "trading-session=" <> token)]
      (expected, _) = handleRequest (opensAt (engineInfo initial)) (playerID (head (webPlayers game))) (SubmitOrder order) initial
  response <- runSession (srequest (post "/orders" headers fields)) (webApplication game)
  actual <- readMVar (runtimeEngine (webRuntime game))
  pure $ conjoin [simpleStatus response === status200, engineBook actual === engineBook expected]

-- Concurrent joins are roster lookup: repetitions all succeed with the same
-- identity, unknown names fail, and the complete engine stays unchanged.
prop_joins :: Property
prop_joins = forAllShrink (listOf1 (elements (map T.pack webPlayerNames ++ ["unknown", "Alice", " alice "])))
  (shrinkList (const [])) $ \names -> ioProperty $ do
    game <- newWebGame 3600
    before <- readMVar (runtimeEngine (webRuntime game))
    outcomes <- mapConcurrently (joinWebPlayer game) names
    sessions <- readTVarIO (webSessions game)
    after <- readMVar (runtimeEngine (webRuntime game))
    let expected name = find ((== T.unpack name) . displayName) (players before)
        selected outcome = either (const Nothing) (fmap sessionPlayer . (`Map.lookup` sessions)) outcome
    pure $ conjoin
      [ map selected outcomes === map expected names
      , map isRight outcomes === map (maybe False (const True) . expected) names
      , sort (nub (map (playerID . sessionPlayer) (Map.elems sessions)))
          === sort (nub [playerID player | name <- names, Just player <- [expected name]])
      , after === before
      ]

-- User text cannot inject SSE fields, including through CR and LF line endings.
prop_sseFraming :: String -> Property
prop_sseFraming value =
  let wire = LBS.toStrict (toLazyByteString (sseHtml "exchange" (toHtml value)))
      lines' = B.split '\n' wire
  in conjoin
    [ head lines' === "event: exchange"
    , property (all (\line -> B.null line || "data: " `B.isPrefixOf` line) (tail lines'))
    , property (not (B.elem '\r' wire))
    , property ("\n\n" `B.isSuffixOf` wire)
    ]

-- Generated names (including HTML metacharacters), secrets and trading results
-- remain associated in the table; no result rows appear while trading.
prop_settlementTable :: Property
prop_settlementTable = forAll genEngine $ \engine ->
  forAll (elements (players engine)) $ \player ->
    let pid = playerID player
        viewAt now = renderHtml (exchangeView (exchangeSnapshot now current) result)
          where
            current = advanceTo now engine
            result = case gamePhase currentSnapshot of
              Trading -> Nothing
              Resolved _ -> Just (settlementFor pid current)
            currentSnapshot = exchangeSnapshot now current
        -- Read the first table's rows as escaped text, independent of styling.
        rows html = map (filter (not . T.null) . map (T.drop 1 . snd . T.breakOn ">") . T.splitOn "<")
          (drop 1 (T.splitOn "<tr>" body))
          where body = fst (T.breakOn "</tbody>" (snd (T.breakOn "<tbody>" (T.decodeUtf8 (LBS.toStrict html)))))
        escaped = T.decodeUtf8 . LBS.toStrict . renderHtml . toHtml
        expected =
          [map escaped [displayName (settledPlayer entry), show (privateNumber (settledPlayer entry)), number (playerPayoff entry)]
          | entry <- playerResults (settlementFor pid (advanceTo (closesAt (engineInfo engine)) engine))]
    in conjoin
      [ rows (viewAt (closesAt (engineInfo engine))) === expected
      , property (not ("Player results" `B.isInfixOf` LBS.toStrict (viewAt (opensAt (engineInfo engine)))))
      ]

-- Rejoining, with or without the original cookie, preserves an account after
-- an arbitrary order; the cookie always selects the named existing player.
prop_rejoin :: Property
prop_rejoin = forAll (elements webPlayerNames) $ \name ->
  forAll genOrder $ \order -> forAll arbitrary $ \sameBrowser -> ioProperty $ do
    game <- newWebGame 3600
    Right token <- joinWebPlayer game (T.pack name)
    let headers = [(hCookie, "trading-session=" <> token)]
    _ <- runSession (srequest (post "/orders" headers (orderFields order))) (webApplication game)
    before <- readMVar (runtimeEngine (webRuntime game))
    joined <- runSession (srequest (post "/join" (if sameBrowser then headers else [])
      [("name", T.encodeUtf8 (T.pack name))])) (webApplication game)
    let cookie = maybe "" (B.takeWhile (/= ';')) (lookup "Set-Cookie" (simpleHeaders joined))
        pageRequest = defaultRequest { requestHeaders = [(hCookie, cookie)] }
    selected <- lookupSession game pageRequest
    page <- runSession (request pageRequest) (webApplication game)
    after <- readMVar (runtimeEngine (webRuntime game))
    pure $ conjoin
      [ simpleStatus joined === status303
      , fmap sessionPlayer selected === find ((== name) . displayName) (players before)
      , property (LBS.toStrict (renderHtml (toHtml ("Playing as " ++ name))) `B.isInfixOf` LBS.toStrict (simpleBody page))
      , after === before
      ]

-- Unknown names are errors even for an already joined browser, and cannot
-- change either the roster/account state or the browser's existing sessions.
prop_unknownPlayer :: Property
prop_unknownPlayer = forAllShrink (arbitrary `suchThat` (`notElem` webPlayerNames))
  (filter (`notElem` webPlayerNames) . shrink) $ \name ->
  forAll arbitrary $ \loggedIn -> ioProperty $ do
    game <- newWebGame 3600
    Right token <- joinWebPlayer game "alice"
    before <- readMVar (runtimeEngine (webRuntime game))
    sessions <- readTVarIO (webSessions game)
    let headers = if loggedIn then [(hCookie, "trading-session=" <> token)] else []
    response <- runSession (srequest (post "/join" headers [("name", T.encodeUtf8 (T.pack name))])) (webApplication game)
    after <- readMVar (runtimeEngine (webRuntime game))
    remaining <- readTVarIO (webSessions game)
    pure $ conjoin
      [ simpleStatus response === status400
      , lookup "Set-Cookie" (simpleHeaders response) === Nothing
      , fmap sessionPlayer remaining === fmap sessionPlayer sessions
      , after === before
      ]

-- An idle HTTP stream receives closure from the shared player's worker, sends
-- its final view, and terminates without another browser command.
prop_streamClosure :: Property
prop_streamClosure = forAll genEngine $ \initial -> forAll (elements (players initial)) $ \player ->
  liveProperty "SSE automatic settlement" $ do
    (clock, advance) <- manualClock (opensAt (engineInfo initial))
    runtime <- newLiveRuntime clock (const (pure ())) initial
    sessions <- newTVarIO Map.empty
    chunks <- newTVarIO []
    let game = WebGame runtime sessions (players initial) []
        (_, _, stream) = responseToStream (eventStream game (BrowserSession player))
        write chunk = atomically (modifyTVar' chunks (++ [toLazyByteString chunk]))
    Async.withAsync (stream (\body -> body write (pure ()))) $ \connection -> do
      atomically (readTVar chunks >>= check . not . null)
      advance (closesAt (engineInfo initial))
      void (runExchange runtime)
      Async.wait connection
      wire <- LBS.concat <$> readTVarIO chunks
      final <- atomically (tryReadTMVar (runtimeFinal runtime))
      pure $ conjoin
        [ property ("event: closed\ndata: done\n\n" `LBS.isSuffixOf` wire)
        , property ("Player results" `B.isInfixOf` LBS.toStrict wire)
        , property (isJust final)
        ]

webProperties :: [Property]
webProperties = [prop_portfolioTable, prop_streamPortfolios, prop_instrumentViews, prop_disabledHttpOrder, prop_orderForm, prop_requiresSession, prop_boundOrder, prop_joins, property prop_sseFraming, prop_settlementTable, prop_rejoin, prop_unknownPlayer, prop_streamClosure]

-- A connected observer receives the complete updated portfolio table after an order.
prop_streamPortfolios :: Property
prop_streamPortfolios = forAll genEngine $ \initial -> forAll genOrder $ \order ->
  forAll (elements (players initial)) $ \trader -> forAll (elements (players initial)) $ \viewer ->
    liveProperty "SSE public portfolios" $ do
      (clock, _) <- manualClock simulationStart
      runtime <- newLiveRuntime clock (const (pure ())) initial
      sessions <- newTVarIO Map.empty
      chunks <- newTVarIO []
      let game = WebGame runtime sessions (players initial) []
          (_, _, stream) = responseToStream (eventStream game (BrowserSession viewer))
          write chunk = atomically (modifyTVar' chunks (++ [toLazyByteString chunk]))
      Async.withAsync (stream (\body -> body write (pure ()))) $ \_ -> do
        atomically (readTVar chunks >>= check . not . null)
        _ <- handleLiveRequest runtime (playerID trader) (SubmitOrder order)
        snapshot <- handleLiveRequest runtime (playerID viewer) GetExchangeState
        let expected = toLazyByteString (sseHtml "exchange" (exchangeView snapshot Nothing))
        atomically (readTVar chunks >>= check . elem expected)
        pure (property ("id=\"portfolios\"" `B.isInfixOf` LBS.toStrict expected))

-- The public table preserves names (including HTML metacharacters), quantities,
-- and exact cash amounts for every roster member, with any instrument selection.
prop_portfolioTable :: Property
prop_portfolioTable = forAll genEngine $ \engine ->
  forAll (Set.fromList <$> sublistOf [minBound .. maxBound]) $ \enabled ->
    let snapshot = (exchangeSnapshot simulationStart engine)
          { gameInfo = (engineInfo engine) { enabledInstruments = enabled } }
        html = T.decodeUtf8 (LBS.toStrict (renderHtml (exchangeView snapshot Nothing)))
        section = snd (T.breakOn "id=\"portfolios\"" html)
        body = fst (T.breakOn "</tbody>" (snd (T.breakOn "<tbody>" section)))
        rows = map (filter (not . T.null) . map (T.drop 1 . snd . T.breakOn ">") . T.splitOn "<")
          (drop 1 (T.splitOn "<tr>" body))
        escaped = T.decodeUtf8 . LBS.toStrict . renderHtml . toHtml
        expected =
          [map escaped ([displayName player]
            ++ [let units = Map.findWithDefault 0 asset (positions account)
                in if units == 0 then "0" else show units ++ " @ $"
                     ++ number (Map.findWithDefault 0 asset (netSpent account) / fromInteger units)
               | asset <- Set.toAscList enabled]
            ++ [number (cash account)])
          | (pid, account) <- Map.toAscList (accounts (engineBook engine))
          , player <- players engine, playerID player == pid]
    in rows === expected

-- Only enabled instruments have books and order choices in a full page or SSE.
prop_instrumentViews :: Property
prop_instrumentViews = forAll genRoster $ \roster ->
  forAll (Set.fromList <$> sublistOf [minBound .. maxBound]) $ \enabled ->
    let engine = newEngineWithInstruments enabled simulationStart 60 roster
        snapshot = exchangeSnapshot simulationStart engine
        player = head roster
        page = LBS.toStrict (renderHtml (gameView "" (BrowserSession player) (privateNumber player) snapshot Nothing))
        wire = LBS.toStrict (toLazyByteString (sseHtml "exchange" (exchangeView snapshot Nothing)))
        contains prefix asset bytes = B.pack (prefix ++ show asset ++ "\"") `B.isInfixOf` bytes
    in conjoin [conjoin
         [ contains "data-instrument=\"" asset page === Set.member asset enabled
         , contains "data-instrument=\"" asset wire === Set.member asset enabled
         , contains "<option value=\"" asset page === Set.member asset enabled ]
       | asset <- Set.toAscList allInstruments ]

-- Hiding a choice is insufficient: crafted requests must also be rejected.
prop_disabledHttpOrder :: Property
prop_disabledHttpOrder = forAll genRoster $ \roster -> forAll genOrder $ \order ->
  forAll (Set.fromList <$> sublistOf [minBound .. maxBound]) $ \selection ->
    liveProperty "disabled HTTP instrument" $ do
      let enabled = Set.delete (instrument order) selection
          initial = newEngineWithInstruments enabled simulationStart 60 roster
          player = head roster
      (clock, _) <- manualClock simulationStart
      runtime <- newLiveRuntime clock (const (pure ())) initial
      sessions <- newTVarIO Map.empty
      let game = WebGame runtime sessions roster []
      Right token <- joinWebPlayer game (T.pack (displayName player))
      response <- runSession (srequest (post "/orders" [(hCookie, "trading-session=" <> token)] (orderFields order))) (webApplication game)
      after <- readMVar (runtimeEngine runtime)
      pure $ conjoin [after === initial, property ("disabled" `B.isInfixOf` LBS.toStrict (simpleBody response))]

prop_orderForm :: Property
prop_orderForm = forAll genOrder $ \order -> parseOrderForm (orderFields order) === Right order
